{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Dcc ( Context (..)
           , Protocol (..)
           , FileMetadata (..)
           , asCtcpMsg
           , rNick
           , isResumable
           , fileMetadata
           , parseDccProtocol
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
import           Data.ByteString.Char8        (ByteString, length, null, pack,
                                               unwords)
import qualified Data.ByteString.Lazy.Char8   as Lazy (toStrict)
import           Data.IP                      (IPv4, toHostAddress)
import           Network.IRC.CTCP
import           Network.Socket               hiding (recv, recvFrom, send,
                                               sendTo)
import           Network.Socket.ByteString
import           Prelude                      hiding (length, null, unwords)
import           System.Console.Concurrent    (outputConcurrent)
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

acceptFile :: Protocol -> Context -> (Int -> IO ()) -> IO ()
acceptFile (Dcc f ip port) _ onChunk = do
    outputConcurrent $ "Connecting to " ++ show ip ++ ":" ++ show port ++ "…\n"
    withFileAsOutput (fileName f) (\file ->
      withActiveSocket ip port $ downloadToFile file onChunk 0)
acceptFile (ReverseDcc f ip t) c onChunk = do
    outputConcurrent $ "Awaiting connection from " ++ show ip ++ "…\n"
    withFileAsOutput (fileName f) (\file ->
      withPassiveSocket ip (sendCmd (connection c) . offerPassiveSink c f t)
        (downloadToFile file onChunk 0))

resumeFile :: Protocol -> Context -> (Int -> IO ()) -> Integer
              -> IO ()
resumeFile (Dcc f ip port) _ onChunk pos = do
    outputConcurrent $ "Connecting to " ++ show ip ++ ":" ++ show port ++ "…\n"
    withFileAsOutputExt (fileName f) AppendMode NoBuffering (\file ->
      withActiveSocket ip port (\con ->
          do outputConcurrent $ "Resuming file at " ++ show pos ++ "…\n"
             downloadToFile file onChunk (fromIntegral pos) con))
resumeFile (ReverseDcc f ip t) c onChunk pos = do
    outputConcurrent $ "Awaiting connection from " ++ show ip ++ "…\n"
    withFileAsOutputExt (fileName f) AppendMode NoBuffering (\file ->
      withPassiveSocket ip (sendCmd (connection c) . offerPassiveSink c f t) (\con ->
          do outputConcurrent $ "Resuming file at " ++ show pos ++ "…\n"
             downloadToFile file onChunk (fromIntegral pos) con))

withActiveSocket ip port onConnected = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (sockAddr ip port)
    onConnected sock
    sClose sock

withPassiveSocket ip onListen onConnected = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
    listen sock 1
    port <- socketPort sock
    onListen port
    accepted <- timeout 10000000 $ accept sock
    case accepted of
      Just (conn, SockAddrInet _ client)
          | client == toHostAddress ip -> do
                              onConnected conn
                              sClose conn
      Just (conn, _) -> sClose conn
      _ -> return ()
    sClose sock

sockAddr ip port = SockAddrInet port (toHostAddress ip)

tryResume :: Protocol -> Context -> Integer -> Command
tryResume p c pos = MPrivmsg (rNick c) $ asCtcpMsg (resumeMsgParams p pos)

asCtcpMsg :: [ByteString] -> ByteString
asCtcpMsg = getUnderlyingByteString . encodeCTCP . unwords

rNick :: Context -> ByteString
rNick = pack . remoteNick

onResumeAccepted :: Protocol -> Nickname -> Broadcast Integer -> EventFunc
onResumeAccepted p remoteNick resumeAccepted _ =
  onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
    case asCTCP mMsg >>= hush . parseAcceptPosition p of
      Just position -> broadcast resumeAccepted position
      _ -> return ())

isResumable :: Context -> Protocol -> ExceptT String IO Integer
isResumable c p =
  let f = fileMetadata p in
  do exists <- lift $ fileExist (fileName f)
     if exists then
       do stats <- lift $ getFileStatus (fileName f)
          let curSize = fromIntegral (Files.fileSize stats)
          if isRegularFile stats then
            if curSize < fileSize f then do
              lift $ putStrLn "Resumable file found."
              sendResumeRequest c curSize p
            else throwE "File already exists and seems complete."
          else do lift $ putStrLn "No resumable file found, starting from zero."
                  return 0
      else do lift $ putStrLn "No resumable file found, starting from zero."
              return 0

sendResumeRequest :: Context -> Integer -> Protocol -> ExceptT String IO Integer
sendResumeRequest c pos p =
    do receivedAcceptMsg <- lift Broadcast.new
       lift $ changeEvents (connection c)
           [ Privmsg (onResumeAccepted p (remoteNick c) receivedAcceptMsg)
           , Notice logMsg ]
       lift $ sendCmd (connection c) (tryResume p c pos)
       ackPos <- lift $ Broadcast.listenTimeout receivedAcceptMsg 30000000
       failWith "Resume was not accepted in time." ackPos

downloadToFile :: File -> (Int -> IO ()) -> Int -> Socket -> IO ()
downloadToFile file onPacket size sock =
  do buffer <- recv sock $ 4096 * 1024
     let received = length buffer
     let receivedTotal = size + received
     unless (null buffer) $
       do onPacket received
          sendNumReceived sock receivedTotal
          Just buffer `write` file
          downloadToFile file onPacket receivedTotal sock

sendNumReceived :: Socket -> Int -> IO ()
sendNumReceived sock num = sendAll sock $ toNetworkByteOrder num

toNetworkByteOrder :: Int -> ByteString
toNetworkByteOrder = Lazy.toStrict . runPut . putWord32be . fromIntegral

offerPassiveSink :: Context -> FileMetadata -> Token -> PortNumber -> Command
offerPassiveSink Context { publicIp = Just ip, remoteNick} f t port =
   MPrivmsg (pack remoteNick) $ asCtcpMsg (sendMsgParams f t ip port)

resumeMsgParams :: Protocol -> Integer -> [ByteString]
resumeMsgParams (Dcc f _ port) pos = "DCC RESUME" :
                                     map pack [ show (fileName f)
                                              , show port
                                              , show pos ]
resumeMsgParams (ReverseDcc f _ t) pos = "DCC SEND" :
                                         map pack [ show (fileName f)
                                                  , show 0
                                                  , show pos
                                                  , show t ]

sendMsgParams :: FileMetadata -> Token -> IPv4 -> PortNumber -> [ByteString]
sendMsgParams f t ip port = "DCC SEND" :
                            map pack [ show (fileName f)
                                     , show ip
                                     , show port
                                     , show (fileSize f)
                                     , show t ]
