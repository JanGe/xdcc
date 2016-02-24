{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Dcc ( FileMetadata (..)
           , doIfCtcp
           , sendMsg
           , canResume
           , resumeFile
           , acceptFile
           , offerSink
           ) where

import           Irc

import           Network.IRC.DCC
import           Network.IRC.DCC.FileTransfer

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listenTimeout, new)
import           Control.Error
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask)
import           Data.ByteString.Char8        (ByteString, pack)
import           Data.IP                      (IPv4)
import           Network.IRC.CTCP
import           Network.Socket               (PortNumber)
import           Path                         (fromRelFile)
import           Prelude                      hiding (length, null)
import           System.Console.Concurrent    (outputConcurrent)
import           System.Posix.Files           (fileExist, getFileStatus,
                                               isRegularFile)
import qualified System.Posix.Files           as Files (fileSize)

import           Network.SimpleIRC            (Command (MPrivmsg), EventFunc,
                                               IrcEvent (Privmsg, Notice),
                                               IrcMessage (..), changeEvents,
                                               mMsg, sendCmd)

sendResumeRequest :: Offer -> FileOffset -> IrcIO FileOffset
sendResumeRequest (Offer tt f) pos =
    do c@Context {connection, remoteNick} <- ask
       let tryResume = TryResume tt f pos
       receivedAcceptMsg <- liftIO Broadcast.new
       liftIO $ changeEvents connection
           [ Privmsg (onResumeAccepted tryResume remoteNick receivedAcceptMsg)
           , Notice logMsg ]
       liftIO $ sendMsg c (resumeCmd tryResume)
       ackPos <- liftIO $ Broadcast.listenTimeout receivedAcceptMsg 30000000
       lift $ failWith "Resume was not accepted in time." ackPos

onResumeAccepted :: TryResume -> Nickname -> Broadcast FileOffset -> EventFunc
onResumeAccepted t remoteNick resumeAccepted _ =
    onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
        case doIfCtcp (runParser (decodeAcceptResume t)) mMsg of
          Right (AcceptResume _ _ pos) -> broadcast resumeAccepted pos
          Left e -> outputConcurrent e)

canResume :: Offer -> IrcIO (Maybe FileOffset)
canResume o@(Offer _ (FileMetadata fn fs)) =
    do curSize <- liftIO $ getFileSizeSafe (fromRelFile fn)
       case curSize of
         Just s
           | s < fs ->
               do liftIO $ outputConcurrent ( "Resumable file found with size "
                                           ++ show s ++ ".\n")
                  Just <$> sendResumeRequest o s
           | otherwise ->
               lift $ throwE "File already exists and seems complete."
         Nothing ->
             do liftIO $ outputConcurrent ( "No resumable file found, starting "
                                         ++ "from zero.\n" :: String)
                return Nothing

getFileSizeSafe :: FilePath -> IO (Maybe FileOffset)
getFileSizeSafe file =
    do exists <- fileExist file
       if exists
          then do stats <- getFileStatus file
                  if isRegularFile stats
                     then return $ Just (fromIntegral (Files.fileSize stats))
                     else return Nothing
          else return Nothing

resumeCmd :: TryResume -> ByteString
resumeCmd = getUnderlyingByteString . encodeTryResume

offerSink :: Offer -> Context -> PortNumber -> ExceptT String IO ()
offerSink (Offer tt f) c p =
    case publicIp c of
      Just i -> case offerSinkCmd tt f i p of
                  Just cmd -> liftIO $ sendMsg c cmd
                  Nothing -> liftIO $ return ()
      Nothing -> throwE ( "Passive connections are only supported, if you "
                       ++ "provide your external IP address on the command "
                       ++ "line using the '--publicIp' option. You could "
                       ++ "also try something like: "
                       ++ "'--publicIP `curl -s https://4.ifcfg.me`'." )

offerSinkCmd :: TransferType -> FileMetadata -> IPv4 -> PortNumber
             -> Maybe ByteString
offerSinkCmd tt f i p = do enc <- encodeOfferSink (OfferSink tt f i p)
                           return (getUnderlyingByteString enc)

doIfCtcp :: (CTCPByteString -> Either String b) -> ByteString -> Either String b
doIfCtcp f mMsg = do msg <- note "Not a CTCP message." (asCTCP mMsg)
                     f msg

sendMsg :: Context -> ByteString -> IO ()
sendMsg Context { connection, remoteNick } msg =
    do sendCmd connection command
       outputConcurrent (show command ++ "\n")
  where command = MPrivmsg (pack remoteNick) msg
