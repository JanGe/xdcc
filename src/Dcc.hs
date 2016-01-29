{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Dcc ( Context (..)
           , Protocol (..)
           , FileMetadata (..)
           , asCtcpMsg
           , rNick
           , parseAcceptMsg
           , downloadToFile
           , isResumable
           , fileMetadata
           , offerParserDcc
           , offerParserReverseDcc
           , resumeFile
           , acceptFile
           ) where

import           Dcc.Parser
import           Dcc.Types
import           Irc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listenTimeout, new)
import           Control.Error
import           Control.Monad                (unless)
import           Control.Monad.Trans.Class    (lift)
import           Data.Binary.Put              (putWord32be, runPut)
import           Data.ByteString.Char8        (ByteString, pack, null, unwords, length)
import qualified Data.ByteString.Lazy.Char8   as Lazy (toStrict)
import           Data.IP                      (toHostAddress)
import           Network.IRC.CTCP
import           Network.Socket               hiding (recv, recvFrom, send,
                                               sendTo)
import           Network.Socket.ByteString
import           Prelude                      hiding (length, null, unwords)
import           System.IO                    (BufferMode (NoBuffering),
                                               IOMode (AppendMode))
import           System.IO.Streams            (withFileAsOutput,
                                               withFileAsOutputExt, write)
import           System.Posix.Files           (FileStatus, fileExist,
                                               getFileStatus, isRegularFile)
import qualified System.Posix.Files           as Files (fileSize)
import           System.Timeout

import           Network.SimpleIRC            (Command (MPrivmsg), EventFunc,
                                               IrcEvent (Privmsg, Notice),
                                               IrcMessage (..), changeEvents,
                                               mMsg, sendCmd)

acceptFile :: Protocol -> Connection -> Context -> (Int -> IO ()) -> IO ()
acceptFile (Dcc f ip port) _ _ onPacket = do
    putStrLn $ "Connecting to " ++ show ip ++ ":" ++ show port ++ "…"
    withFileAsOutput (fileName f) (\file ->
      withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (sockAddr ip port)
        downloadToFile sock file onPacket 0
        sClose sock)
acceptFile p@(ReverseDcc f ip t) con c onChunk = do
    putStrLn $ "Awaiting connection from " ++ show ip ++ "…"
    withFileAsOutput (fileName f) (\file ->
      withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
        listen sock 1
        port <- socketPort sock
        sendCmd con (offerPassiveSink p c port)
        accepted <- timeout 10000000 $ accept sock
        case accepted of
          Just (conn, SockAddrInet _ client)
              | client == toHostAddress ip -> do
                                  downloadToFile conn file onChunk 0
                                  sClose conn
          Just (conn, _) -> sClose conn
          _ -> return ()
        sClose sock)

resumeFile :: Protocol -> Connection -> Context -> (Int -> IO ()) -> Integer
              -> IO ()
resumeFile (Dcc f ip port) con _ onChunk pos = do
    putStrLn $ "Connecting to " ++ show ip ++ ":" ++ show port ++ "…"
    withFileAsOutputExt (fileName f) AppendMode NoBuffering (\file ->
      withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (sockAddr ip port)
        downloadToFile sock file onChunk (fromIntegral pos)
        sClose sock)
resumeFile p@(ReverseDcc f ip t) con c onChunk pos = do
    putStrLn $ "Awaiting connection from " ++ show ip ++ "…"
    withFileAsOutputExt (fileName f) AppendMode NoBuffering (\file ->
      withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
        listen sock 1
        port <- socketPort sock
        sendCmd con (offerPassiveSink p c port)
        accepted <- timeout 10000000 $ accept sock
        case accepted of
          Just (conn, SockAddrInet _ client)
              | client == toHostAddress ip -> do
                            putStrLn $ "Resuming file at " ++ show pos ++ "…"
                            downloadToFile conn file onChunk $ fromIntegral pos
                            sClose conn
          Just (conn, _) -> sClose conn
          _ -> return ()
        sClose sock)

sockAddr ip port = SockAddrInet port (toHostAddress ip)

tryResume :: Protocol -> Context -> Integer -> Command
tryResume (Dcc f p _) c pos =
    MPrivmsg (rNick c) $ asCtcpMsg (resumeMsgParamsDcc f p pos)
tryResume (ReverseDcc f s t) c pos =
    MPrivmsg (rNick c) $ asCtcpMsg (resumeMsgParamsRDcc f pos t)

asCtcpMsg :: [ByteString] -> ByteString
asCtcpMsg = getUnderlyingByteString . encodeCTCP . unwords

rNick :: Context -> ByteString
rNick = pack . remoteNick

fileMetadata :: Protocol -> FileMetadata
fileMetadata (Dcc f _ _) = f
fileMetadata (ReverseDcc f _ _) = f

parseAcceptMsg :: Protocol -> Nickname -> Broadcast Integer -> EventFunc
parseAcceptMsg protocol remoteNick instructionsReceived _ =
  onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
    case parseAcceptPosition protocol (asCTCP mMsg) of
      Just position -> broadcast instructionsReceived position
      _ -> return ())

parseAcceptPosition :: Protocol -> Maybe CTCPByteString -> Maybe Integer
parseAcceptPosition p = (=<<) $ parseAccept p . decodeCTCP

isResumable :: Connection -> Context -> Protocol
               -> ExceptT String IO Integer
isResumable con c p =
  let f = fileMetadata p in
  do exists <- lift $ fileExist (fileName f)
     if exists then
       do stats <- lift $ getFileStatus (fileName f)
          let curSize = fromIntegral (Files.fileSize stats)
          if isRegularFile stats then
            if curSize < fileSize f then do
              lift $ putStrLn "Resumable file found."
              sendResumeRequest con c curSize p
            else throwE "File already exists and seems complete."
          else do lift $ putStrLn "No resumable file found, starting from zero."
                  return 0
      else do lift $ putStrLn "No resumable file found, starting from zero."
              return 0

sendResumeRequest :: Connection -> Context -> Integer -> Protocol
                     -> ExceptT String IO Integer
sendResumeRequest con c pos p =
    do receivedAcceptMsg <- lift Broadcast.new
       lift $ changeEvents con
           [ Privmsg (parseAcceptMsg p (remoteNick c) receivedAcceptMsg)
           , Notice logMsg ]
       lift $ sendCmd con (tryResume p c pos)
       ackPos <- lift $ Broadcast.listenTimeout receivedAcceptMsg 30000000
       failWith "Resume was not accepted in time." ackPos

downloadToFile :: Socket -> File -> (Int -> IO ()) -> Int -> IO ()
downloadToFile sock file onPacket size =
  do buffer <- recv sock $ 4096 * 1024
     let received = length buffer
     let receivedTotal = size + received
     unless (null buffer) $
       do onPacket received
          sendNumReceived sock receivedTotal
          Just buffer `write` file
          downloadToFile sock file onPacket receivedTotal

sendNumReceived :: Socket -> Int -> IO ()
sendNumReceived sock num = sendAll sock $ toNetworkByteOrder num

toNetworkByteOrder :: Int -> ByteString
toNetworkByteOrder = Lazy.toStrict . runPut . putWord32be . fromIntegral


offerPassiveSink :: Protocol -> Context -> PortNumber -> Command
offerPassiveSink (ReverseDcc f _ t) c@Context { publicIp = Just h } p =
   MPrivmsg (rNick c) $ asCtcpMsg (sendMsgParams f h p t)
offerPassiveSink _ _ _ = undefined

resumeMsgParamsDcc f p pos = "DCC RESUME" : map pack [ show (fileName f)
                                                     , show p
                                                     , show pos ]

resumeMsgParamsRDcc f pos t = "DCC SEND" : map pack [ show (fileName f)
                                                    , show 0
                                                    , show pos
                                                    , show t ]

sendMsgParams f h p t = "DCC SEND" : map pack [ show (fileName f)
                                              , show h
                                              , show p
                                              , show (fileSize f)
                                              , show t ]
