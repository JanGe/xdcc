module Dcc ( module Irc
           , module Network.IRC.DCC
           , DccIO
           , DccEnv(..)
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
import           Control.Monad.Trans.Reader   (ReaderT, ask)
import           Data.ByteString.Char8        (ByteString)
import           Data.IP                      (IPv4)
import           Network.IRC.CTCP             (getUnderlyingByteString)
import           Network.IRC.DCC
import           Network.IRC.DCC.FileTransfer
import           Network.Socket               (PortNumber)
import           Path                         (fromRelFile)
import           Prelude                      hiding (length, null)
import           System.Console.Concurrent    (outputConcurrent)
import           System.PosixCompat.Files     (fileExist, getFileStatus,
                                               isRegularFile)
import qualified System.PosixCompat.Files     as Files (fileSize)

type DccIO = ReaderT DccEnv IrcIO

data DccEnv = DccEnv { connection :: Connection
                     , remoteNick :: Nickname
                     , publicIp   :: Maybe IPv4
                     , localPort  :: Maybe PortNumber }

sendResumeRequest :: OfferFile -> FileOffset -> DccIO FileOffset
sendResumeRequest (OfferFile tt f) pos = do
    let tryResume = TryResumeFile tt f pos
    env <- ask
    lift $ sendAndWaitForAck (connection env)
                             (remoteNick env)
                             (asByteString tryResume)
                             (onResumeAccepted tryResume)
                             "Timeout when waiting for resume"

onResumeAccepted :: TryResumeFile -> Nickname -> Broadcast FileOffset
                 -> EventFunc
onResumeAccepted t rNick resumeAccepted _ =
    onCtcpMessage (from rNick) (\ msg ->
        case runParser (parseAcceptResumeFile t) msg of
          Right (AcceptResumeFile _ _ pos) -> broadcast resumeAccepted pos
          Left e -> outputConcurrent e )

canResume :: OfferFile -> DccIO (Maybe FileOffset)
canResume o@(OfferFile _ (FileMetadata fn totalSize)) = do
    curSize <- liftIO $ getFileSizeSafe (fromRelFile fn)
    case (curSize, totalSize) of
      (Just pos, Just total)
        | pos < total -> do
            liftIO $ outputConcurrent
                ( "Resumable file found with size " ++ show pos ++ ".\n" )
            Just <$> sendResumeRequest o pos
        | otherwise ->
            lift $ throwE "File already exists and seems complete."
      (Just _, Nothing) ->
          lift $ throwE "File already exists. Resuming not supported."
      (Nothing, _) -> do
          liftIO $ outputConcurrent
              "No resumable file found, starting from zero.\n"
          return Nothing

getFileSizeSafe :: FilePath -> IO (Maybe FileOffset)
getFileSizeSafe file = do
    exists <- fileExist file
    if exists
       then do stats <- getFileStatus file
               if isRegularFile stats
                  then return $ Just (fromIntegral (Files.fileSize stats))
                  else return Nothing
       else return Nothing

offerSink :: DccEnv -> OfferFile -> PortNumber -> IrcIO ()
offerSink env (OfferFile (Passive _ t) f) p =
    case publicIp env of
      Just i -> lift $ send (connection env)
                            (remoteNick env)
                            (asByteString (OfferFileSink t f i p))
      Nothing -> throwE ( "Passive connections are only supported if you "
                       ++ "provide your external IP address on the command "
                       ++ "line using the '--public-ip' option. You could "
                       ++ "also try something like: "
                       ++ "'--public-ip `curl -s https://4.ifcfg.me`'." )
-- Only passive connections can offer a sink to connect to
offerSink _ _ _ = lift $ return ()

asByteString :: CtcpCommand a => a -> ByteString
asByteString = getUnderlyingByteString . encodeCtcp
