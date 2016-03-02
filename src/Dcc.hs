module Dcc ( module Irc
           , module Network.IRC.DCC
           , FileMetadata (..)
           , canResume
           , resumeFile
           , acceptFile
           , offerSink
           ) where

import           Irc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import           Control.Error
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (asks)
import           Data.ByteString.Char8        (ByteString)
import           Network.IRC.CTCP             (getUnderlyingByteString)
import           Network.IRC.DCC
import           Network.IRC.DCC.FileTransfer
import           Network.Socket               (PortNumber)
import           Path                         (fromRelFile)
import           Prelude                      hiding (length, null)
import           System.Console.Concurrent    (outputConcurrent)
import           System.Posix.Files           (fileExist, getFileStatus,
                                               isRegularFile)
import qualified System.Posix.Files           as Files (fileSize)

sendResumeRequest :: Offer -> FileOffset -> IrcIO FileOffset
sendResumeRequest (Offer tt f) pos =
    let tryResume = TryResume tt f pos in
    do rNick <- asks remoteNick
       sendAndWaitForAck (resumeCmd tryResume)
                         (onResumeAccepted tryResume rNick)
                         "Timeout when waiting for resume"

onResumeAccepted :: TryResume -> Nickname -> Broadcast FileOffset -> EventFunc
onResumeAccepted t rNick resumeAccepted _ =
    onCtcpMessage (from rNick) (\ msg ->
        case runParser (decodeAcceptResume t) msg of
          Right (AcceptResume _ _ pos) -> broadcast resumeAccepted pos
          Left e -> outputConcurrent e )

canResume :: Offer -> IrcIO (Maybe FileOffset)
canResume o@(Offer _ (FileMetadata fn (Just fs))) =
    do curSize <- liftIO $ getFileSizeSafe (fromRelFile fn)
       case curSize of
         Just s
           | s < fs -> do
               liftIO $ outputConcurrent
                          ("Resumable file found with size " ++ show s ++ ".\n")
               Just <$> sendResumeRequest o s
           | otherwise ->
               lift $ throwE "File already exists and seems complete."
         Nothing ->
             do liftIO $ outputConcurrent
                           "No resumable file found, starting from zero.\n"
                return Nothing
canResume _ = lift $ throwE "File already exists. Resuming not supported."

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
      Just i -> lift $ sequence_ (send c <$> offerSinkCmd (OfferSink tt f i p))
      Nothing -> throwE ( "Passive connections are only supported, if you "
                       ++ "provide your external IP address on the command "
                       ++ "line using the '--publicIp' option. You could "
                       ++ "also try something like: "
                       ++ "'--publicIP `curl -s https://4.ifcfg.me`'." )

offerSinkCmd :: OfferSink -> Maybe ByteString
offerSinkCmd os = getUnderlyingByteString <$> encodeOfferSink os
