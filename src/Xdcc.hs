{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Xdcc (Instructions(..), acceptFile, sendFileRequest) where

import           Irc

import           Control.Concurrent.Broadcast (Broadcast, broadcast, listen,
                                               new)
import           Control.Monad                (unless)
import           Data.Binary                  (byteSwap32)
import           Data.Binary.Put              (putWord32be, runPut)
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8   as Lazy
import           Network.Socket               hiding (listen, recv, recvFrom,
                                               send, sendTo)
import           Network.Socket.ByteString
import           Prelude                      hiding (and, length, null,
                                               splitAt)
import           System.FilePath              (takeFileName)
import           System.IO.Streams            (OutputStream, withFileAsOutput,
                                               write)
import           Text.Parse.ByteString        (literal, many1Satisfy, onFail,
                                               parseUnsignedInteger, runParser)

import           Network.SimpleIRC            (Command (MPrivmsg), EventFunc,
                                               IrcEvent (Privmsg), changeEvents,
                                               mMsg, sendCmd)

data Instructions = Instructions { host     :: HostAddress
                                 , port     :: PortNumber
                                 , fileName :: FilePath
                                 , fileSize :: Maybe Integer }
    deriving (Show)

type File = OutputStream ByteString

sendFileRequest :: Connection -> Nickname -> Pack -> (Instructions -> IO ())
                   -> IO Instructions
sendFileRequest connection remoteNick num onReceive =
    do receivedInstructions <- new
       changeEvents connection [
           Privmsg $ onDccSendFrom rNick receivedInstructions ]
       sendCmd connection sendMessage
       instructions <- listen receivedInstructions
       onReceive instructions
       return instructions
  where rNick = pack remoteNick
        sendMessage = MPrivmsg rNick $ "xdcc send #" `append` pack (show num)

acceptFile :: Instructions -> IO () -> (Int -> IO ())  -> IO ()
acceptFile Instructions {..} onConnected onPacket =
   withSocketsDo . withFileAsOutput fileName $ (\file ->
      do ip <- inet_ntoa host
         sock <- socket AF_INET Stream defaultProtocol
         connect sock $ SockAddrInet port host
         onConnected
         downloadToFile sock file onPacket 0
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
sendNumReceived sock num = sendAll sock (toNetworkByteOrder num)

toNetworkByteOrder :: Int -> ByteString
toNetworkByteOrder = Lazy.toStrict . runPut . putWord32be . fromIntegral

onDccSendFrom :: ByteString -> Broadcast Instructions -> EventFunc
onDccSendFrom rNick instructionsReceived _ ircMessage =
  onMessageDo (from rNick `and` msgHasPrefix dccSendPrefix) (\_ ->
      case parseInstructions parameters of
         Left e -> ioError $ userError e
         Right instructions -> broadcast instructionsReceived instructions)
      ircMessage
  where dccSendPrefix = "\SOHDCC SEND "
        (_, parameters) = splitAt (length dccSendPrefix) $ mMsg ircMessage

parseInstructions :: ByteString -> Either String Instructions
parseInstructions msg = case result of
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
                      fileSize <- (Just <$> do literal " "
                                               parseUnsignedInteger)
                                  `onFail` return Nothing
                      return Instructions {
                          host = byteSwap32 $ fromIntegral ip,
                          port = fromInteger port,
                          fileName = takeFileName $ Lazy.unpack fileName,
                          fileSize = fileSize
                      }
