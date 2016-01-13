{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Xdcc (Parameters(..), acceptFile, sendFileRequest, isResumable,
  resumeFile) where

import           Irc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listen, new)
import           Control.Monad                (unless)
import           Data.Binary                  (byteSwap32)
import           Data.Binary.Put              (putWord32be, runPut)
import           Data.ByteString.Builder
import           Data.ByteString.Char8        hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8   as Lazy
import           Data.Monoid                  ((<>))
import           GHC.IO.Handle                (BufferMode (NoBuffering))
import           Network.Socket               hiding (recv, recvFrom, send,
                                               sendTo)
import           Network.Socket.ByteString
import           Prelude                      hiding (and, length, null,
                                               splitAt)
import           System.FilePath              (takeFileName)
import           System.IO                    (IOMode (AppendMode))
import           System.IO.Streams            (OutputStream, withFileAsOutput,
                                               withFileAsOutputExt, write)
import           System.Posix.Files           (FileStatus, fileExist,
                                               getFileStatus, isRegularFile)
import qualified System.Posix.Files           as Files (fileSize)
import           System.Timeout
import           Text.Parse.ByteString        (literal, many1Satisfy, onFail,
                                               parseUnsignedInteger, runParser)

import           Network.SimpleIRC            (Command (MPrivmsg), EventFunc,
                                               IrcEvent (Privmsg), changeEvents,
                                               mMsg, sendCmd)

data Parameters = DccSend { host     :: HostAddress
                          , port     :: PortNumber
                          , fileName :: FilePath
                          , fileSize :: Integer } |
                  ReverseDcc { remoteNick :: ByteString
                             , publicIp   :: HostAddress
                             , host       :: HostAddress
                             , fileName   :: FilePath
                             , fileSize   :: Integer
                             , token      :: ByteString } |
                  DccSendResume { host     :: HostAddress
                                , port     :: PortNumber
                                , fileName :: FilePath
                                , fileSize :: Integer
                                , position :: Integer }
    deriving (Show)

type File = OutputStream ByteString

dccSendPrefix = "\SOHDCC SEND "
dccResumePrefix = "\SOHDCC RESUME "
dccAcceptPrefix = "\SOHDCC ACCEPT "
dccSuffix = "\SOH"

sendFileRequest :: Connection -> HostAddress -> Nickname -> Pack ->
                   (Parameters -> IO ())
                   -> IO Parameters
sendFileRequest connection publicIp remoteNick num onReceive =
    do receivedInstructions <- Broadcast.new
       changeEvents connection (
           Privmsg (onDccSendFrom rNick publicIp receivedInstructions)
           : defaultEvents)
       sendMsgTo connection remoteNick message
       instructions <- Broadcast.listen receivedInstructions
       onReceive instructions
       return instructions
  where rNick = pack remoteNick
        message = "xdcc send #" `append` pack (show num)

sendResumeRequest :: Connection -> Integer -> Nickname -> Parameters
                     -> IO Parameters
sendResumeRequest connection position remoteNick DccSend{..} =
    do receivedInstructions <- Broadcast.new
       changeEvents connection (
           Privmsg (onDccAcceptFrom rNick receivedInstructions)
           : defaultEvents)
       sendCmd connection resumeCmd
       pos <- Broadcast.listen receivedInstructions
       return DccSendResume { host = host
                            , port = port
                            , fileName = fileName
                            , fileSize = fileSize
                            , position = pos
                            }
  where rNick = pack remoteNick
        resumeCmd = MPrivmsg rNick $ dccResumePrefix <> pack (
                                                    fileName ++ " " ++
                                                    show port ++ " " ++
                                                    show position)
                                                    <> dccSuffix

acceptFile :: Connection -> Parameters -> (Int -> IO ())  -> IO ()
acceptFile _ DccSend {..} onPacket =
  withFileAsOutput fileName (\file -> withSocketsDo $
      do sock <- socket AF_INET Stream defaultProtocol
         connect sock $ SockAddrInet port host
         downloadToFile sock file onPacket 0
         sClose sock)
acceptFile connection ReverseDcc {..} onPacket =
  let acceptCmd port = MPrivmsg remoteNick $ dccSendPrefix <> pack (
                                            fileName ++ " " ++
                                            show (byteSwap32 publicIp) ++ " " ++
                                            show port ++ " " ++
                                            show fileSize ++ " ")
                                            <> token
                                            <> dccSuffix in
  withFileAsOutput fileName (\file -> withSocketsDo $
      do sock <- socket AF_INET Stream defaultProtocol
         bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
         listen sock 1
         port <- socketPort sock
         sendCmd connection $ acceptCmd port
         accepted <- timeout 10000000 $ accept sock
         case accepted of
           Just (conn, SockAddrInet _ client)
               | client == host -> do downloadToFile conn file onPacket 0
                                      sClose conn
           Just (conn, _) -> sClose conn
           _ -> return ()
         sClose sock)

isResumable :: Connection -> Nickname -> Parameters -> IO Parameters
isResumable connection remoteNick instructions@DccSend {..} =
  do exists <- fileExist fileName
     if exists then
       do stats <- getFileStatus fileName
          let curSize = fromIntegral $ Files.fileSize stats
          if isRegularFile stats && curSize < fileSize
             then do putStrLn "Found file..."
                     sendResumeRequest connection curSize remoteNick instructions
             else return instructions
     else return instructions

resumeFile :: Connection -> Parameters -> (Int -> IO ())  -> IO ()
resumeFile _ DccSendResume {..} onPacket =
 withFileAsOutputExt fileName AppendMode NoBuffering (\file -> withSocketsDo $
     do sock <- socket AF_INET Stream defaultProtocol
        connect sock $ SockAddrInet port host
        downloadToFile sock file onPacket $ fromInteger position
        sClose sock)

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

onDccSendFrom :: ByteString -> HostAddress -> Broadcast Parameters -> EventFunc
onDccSendFrom rNick publicIp instructionsReceived _ ircMessage =
  onMessageDo (from rNick `and` msgHasPrefix dccSendPrefix) (\_ ->
      case parseInstructions parameters publicIp rNick of
         Left e -> ioError $ userError e
         Right instructions -> broadcast instructionsReceived instructions)
      ircMessage
  where (_, parameters) = splitAt (length dccSendPrefix) $ mMsg ircMessage

onDccAcceptFrom :: ByteString -> Broadcast Integer -> EventFunc
onDccAcceptFrom rNick instructionsReceived _ ircMessage =
    onMessageDo (from rNick `and` msgHasPrefix dccAcceptPrefix) (\_ ->
        case parseAcceptMsg parameters of
           Left e -> ioError $ userError e
           Right position -> broadcast instructionsReceived position)
        ircMessage
    where (_, parameters) = splitAt (length dccAcceptPrefix) $ mMsg ircMessage

parseAcceptMsg :: ByteString -> Either String Integer
parseAcceptMsg msg = case result of
        (Left error, unconsumed) ->
             Left ("Encountered error: " ++ error ++
                   " when parsing: " ++ Lazy.unpack unconsumed)
        (Right offer, _) -> Right offer
    where result = runParser parser $ Lazy.fromStrict msg
          parser = do _ <- many1Satisfy (/= ' ')
                      literal " "
                      _ <- parseUnsignedInteger
                      literal " "
                      parseUnsignedInteger

parseInstructions :: ByteString -> HostAddress -> ByteString
                     -> Either String Parameters
parseInstructions msg publicIp remoteNick = case result of
        (Left error, unconsumed) ->
             Left ("Encountered error: " ++ error ++
                   " when parsing: " ++ Lazy.unpack unconsumed)
        (Right offer, _) -> Right offer
    where result = runParser parser $ Lazy.fromStrict msg
          parser = do fileName <- many1Satisfy (/= ' ')
                      literal " "
                      ip <- parseUnsignedInteger
                      literal " "
                      port <- parseUnsignedInteger
                      literal " "
                      fileSize <- parseUnsignedInteger
                      if port /= 0 then
                        return DccSend {
                          host = byteSwap32 $ fromIntegral ip,
                          port = fromInteger port,
                          fileName = takeFileName $ Lazy.unpack fileName,
                          fileSize = fileSize
                        }
                      else do literal " "
                              token <- many1Satisfy (/= '\SOH')
                              return ReverseDcc {
                                  remoteNick = remoteNick,
                                  publicIp = publicIp,
                                  host = byteSwap32 $ fromIntegral ip,
                                  fileName = takeFileName $ Lazy.unpack fileName,
                                  fileSize = fileSize,
                                  token = Lazy.toStrict token
                              }
