{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Xdcc (Parameters(..), acceptFile, sendFileRequest) where

import           Irc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listen, new)
import           Control.Monad                (unless)
import           Data.Binary                  (byteSwap32)
import           Data.Binary.Put              (putWord32be, runPut)
import           Data.ByteString.Builder
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8   as Lazy
import           Data.Monoid                  ((<>))
import           Network.Socket               hiding (recv, recvFrom, send,
                                               sendTo)
import           Network.Socket.ByteString
import           Prelude                      hiding (and, length, null,
                                               splitAt)
import           System.FilePath              (takeFileName)
import           System.IO.Streams            (OutputStream, withFileAsOutput,
                                               write)
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
                             , token      :: ByteString }
    deriving (Show)

type File = OutputStream ByteString

dccSendPrefix = "\SOHDCC SEND "
dccSuffix = "\SOH"

sendFileRequest :: Connection -> HostAddress -> Nickname -> Pack ->
                   (Parameters -> IO ())
                   -> IO Parameters
sendFileRequest connection publicIp remoteNick num onReceive =
    do receivedInstructions <- Broadcast.new
       changeEvents connection [
           Privmsg $ onDccSendFrom rNick publicIp receivedInstructions ]
       sendCmd connection sendMessage
       instructions <- Broadcast.listen receivedInstructions
       onReceive instructions
       return instructions
  where rNick = pack remoteNick
        sendMessage = MPrivmsg rNick $ "xdcc send #" `append` pack (show num)

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
