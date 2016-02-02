{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Dcc ( Protocol (..)
           , FileMetadata (..)
           , doIfCtcp
           , sendMsg
           , canResumeFrom
           , fileMetadata
           , parseSendAction
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
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask)
import           Data.Binary.Put              (putWord32be, runPut)
import           Data.ByteString.Char8        (ByteString, length, null, pack,
                                               unwords)
import qualified Data.ByteString.Lazy.Char8   as Lazy (toStrict)
import           Data.IP                      (IPv4, fromHostAddress,
                                               toHostAddress)
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
import           System.Posix.Files           (fileExist, getFileStatus,
                                               isRegularFile)
import qualified System.Posix.Files           as Files (fileSize)
import           System.Timeout

import           Network.SimpleIRC            (Command (MPrivmsg), EventFunc,
                                               IrcEvent (Privmsg, Notice),
                                               IrcMessage (..), changeEvents,
                                               mMsg, sendCmd)

acceptFile :: Protocol -> Context -> (Int -> IO ()) -> IO ()
acceptFile (Dcc f ip port) _ onChunk = do
    outputConcurrent ("Connecting to " ++ show ip ++ ":" ++ show port ++ "…\n")
    withFileAsOutput (fileName f) (\file ->
      withActiveSocket ip port $ downloadToFile file onChunk 0)
acceptFile (ReverseDcc f ip t) c onChunk = do
    outputConcurrent ("Awaiting connection from " ++ show ip ++ "…\n")
    withFileAsOutput (fileName f) (\file ->
      withPassiveSocket ip (sendMsg c . offerPassiveSink c f t)
        (downloadToFile file onChunk 0))

resumeFile :: Protocol -> Context -> (Int -> IO ()) -> Integer -> IO ()
resumeFile (Dcc f ip port) _ onChunk pos = do
    outputConcurrent ("Connecting to " ++ show ip ++ ":" ++ show port ++ "…\n")
    withFileAppending (fileName f) (\file ->
      withActiveSocket ip port (\con ->
          do outputConcurrent $ "Resuming file at " ++ show pos ++ "…\n"
             downloadToFile file onChunk (fromIntegral pos) con))
resumeFile (ReverseDcc f ip t) c onChunk pos = do
    outputConcurrent ("Awaiting connection from " ++ show ip ++ "…\n")
    withFileAppending (fileName f)  (\file ->
      withPassiveSocket ip (sendMsg c . offerPassiveSink c f t) (\con ->
          do outputConcurrent ("Resuming file at " ++ show pos ++ "…\n")
             downloadToFile file onChunk (fromIntegral pos) con))

withFileAppending :: FilePath -> (File -> IO a) -> IO a
withFileAppending fn = withFileAsOutputExt fn AppendMode NoBuffering

withActiveSocket :: IPv4 -> PortNumber -> (Socket -> IO ()) -> IO ()
withActiveSocket ip port onConnected = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (sockAddr ip port)
    onConnected sock
    sClose sock

withPassiveSocket :: IPv4 -> (PortNumber -> IO ()) -> (Socket -> IO ()) -> IO ()
withPassiveSocket ip onListen onConnected = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
    listen sock 1
    port <- socketPort sock
    onListen port
    accepted <- timeout 10000000 $ accept sock
    case accepted of
      Just (conn, SockAddrInet _ client)
        | client == toHostAddress ip -> do onConnected conn
                                           sClose conn
        | otherwise -> do outputConcurrent ( "Expected connection from host "
                                          ++ show (fromHostAddress client)
                                          ++ ", not from " ++ show ip
                                          ++". Aborting…\n" )
                          sClose conn
      _ -> outputConcurrent ( "Timeout when waiting for other party to connect "
                           ++ "on port " ++ show port ++ "…\n")
    sClose sock

sockAddr :: IPv4 -> PortNumber -> SockAddr
sockAddr ip port = SockAddrInet port (toHostAddress ip)

sendResumeRequest :: Protocol -> Integer -> IrcIO Integer
sendResumeRequest p pos =
    do c@Context {connection, remoteNick} <- ask
       receivedAcceptMsg <- liftIO Broadcast.new
       liftIO $ changeEvents connection
           [ Privmsg (onResumeAccepted p remoteNick receivedAcceptMsg)
           , Notice logMsg ]
       liftIO $ sendMsg c (resumeCmd p pos)
       ackPos <- liftIO $ Broadcast.listenTimeout receivedAcceptMsg 30000000
       lift $ failWith "Resume was not accepted in time." ackPos

onResumeAccepted :: Protocol -> Nickname -> Broadcast Integer -> EventFunc
onResumeAccepted p remoteNick resumeAccepted _ =
    onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
        case doIfCtcp (parseAcceptAction p) mMsg of
          Right position -> broadcast resumeAccepted position
          Left e -> outputConcurrent e)

canResumeFrom :: Protocol -> IrcIO Integer
canResumeFrom p =
    do curSize <- liftIO $ getFileSizeSafe (fileName file)
       case curSize of
         Just s
           | s < fileSize file ->
               do liftIO $ outputConcurrent ( "Resumable file found with size "
                                           ++ show s ++ ".\n")
                  sendResumeRequest p s
           | otherwise ->
               lift $ throwE "File already exists and seems complete."
         Nothing ->
             do liftIO $ outputConcurrent ( "No resumable file found, starting "
                                         ++ "from zero.\n" :: String)
                return 0
  where file = fileMetadata p

getFileSizeSafe :: FilePath -> IO (Maybe Integer)
getFileSizeSafe file =
    do exists <- fileExist file
       if exists
          then do stats <- getFileStatus file
                  if isRegularFile stats
                     then return $ Just (fromIntegral (Files.fileSize stats))
                     else return Nothing
          else return Nothing

downloadToFile :: File -> (Int -> IO ()) -> Int -> Socket -> IO ()
downloadToFile file onChunk size sock =
  do buffer <- recv sock $ 4096 * 1024
     unless (null buffer) $
            do let received = length buffer
               onChunk received
               let receivedTotal = size + received
               sendNumReceived sock receivedTotal
               Just buffer `write` file
               downloadToFile file onChunk receivedTotal sock

sendNumReceived :: Socket -> Int -> IO ()
sendNumReceived sock num = sendAll sock $ toNetworkByteOrder num

toNetworkByteOrder :: Int -> ByteString
toNetworkByteOrder = Lazy.toStrict . runPut . putWord32be . fromIntegral

resumeCmd :: Protocol -> Integer -> ByteString
resumeCmd p pos = asCtcpMsg (resumeMsgParams p pos)

offerPassiveSink :: Context -> FileMetadata -> Token -> PortNumber -> ByteString
offerPassiveSink Context { publicIp = Just ip} f t port =
    asCtcpMsg (sendMsgParams f t ip port)
offerPassiveSink _ _ _ _ = undefined

asCtcpMsg :: [ByteString] -> ByteString
asCtcpMsg = getUnderlyingByteString . encodeCTCP . unwords

doIfCtcp :: (CTCPByteString -> Either String b) -> ByteString -> Either String b
doIfCtcp f mMsg = do msg <- note "Not a CTCP message." (asCTCP mMsg)
                     f msg

sendMsg :: Context -> ByteString -> IO ()
sendMsg Context { connection, remoteNick } msg =
    do sendCmd connection command
       outputConcurrent (show command ++ "\n")
  where command = MPrivmsg (pack remoteNick) msg

resumeMsgParams :: Protocol -> Integer -> [ByteString]
resumeMsgParams (Dcc f _ port) pos = "DCC RESUME" :
                                     map pack [ fileName f
                                              , show port
                                              , show pos ]
resumeMsgParams (ReverseDcc f _ t) pos = "DCC RESUME" :
                                         map pack [ fileName f
                                                  , "0"
                                                  , show pos
                                                  , t ]

sendMsgParams :: FileMetadata -> Token -> IPv4 -> PortNumber -> [ByteString]
sendMsgParams f t ip port = "DCC SEND" :
                            map pack [ fileName f
                                     , show (ipToNetworkByteOrder ip)
                                     , show port
                                     , show (fileSize f)
                                     , t ]
