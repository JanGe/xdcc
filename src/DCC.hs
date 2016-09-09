{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module DCC ( module Network.IRC.DCC
           , DccIO
           , runDccIO
           , Env(..)
           , Status(..)
           , offerReceivedHandler
           , acceptResumeHandler
           ) where

import           IRC.Types

import           Control.Error                       (note)
import           Control.Exception.Safe              (MonadThrow)
import           Control.Monad                       (when)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Reader          (ReaderT, ask, asks,
                                                      runReaderT)
import           Data.IP                             (IPv4)
import           Data.Monoid                         ((<>))
import qualified Data.Text                           as T (Text)
import qualified Network.IRC.Client                  as IRC
import           Network.IRC.DCC                     hiding (Path)
import           Network.IRC.DCC.Client.FileTransfer
import           Network.Socket                      (PortNumber)
import           Path                                (File, Path, Rel,
                                                      fromRelFile)
import           Prelude                             hiding (length, null)
import           System.Console.AsciiProgress        (ProgressBar)
import           System.Console.Concurrent           (outputConcurrent)
import           System.PosixCompat.Files            (fileExist, getFileStatus,
                                                      isRegularFile)
import qualified System.PosixCompat.Files            as Files (fileSize)

newtype DccIO s a = DccIO (ReaderT (Env s) (ReaderT (IRC.IRCState s) IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

runDccIO :: Env s -> DccIO s a -> ReaderT (IRC.IRCState s) IO a
runDccIO env (DccIO m) = runReaderT m env

send :: IRC.UnicodeMessage -> DccIO s ()
send msg = DccIO $ do
    send' <- asks sendFn
    lift $ send' msg

putDccState :: Status -> DccIO s ()
putDccState s = DccIO $ do
    putState <- asks putDccStateFn
    lift $ putState s

onDone :: DccIO s ()
onDone = DccIO $ do
    onDone' <- asks onDoneFn
    lift onDone'

onAbort :: DccIO s ()
onAbort = DccIO $ do
    onAbort' <- asks onAbortFn
    lift onAbort'

getEnv :: DccIO s (Env s)
getEnv = DccIO ask

transfer' :: FileTransfer (ReaderT (IRC.IRCState s) IO) -> DccIO s ()
transfer' = DccIO . lift . transfer

class FileOffer a where
  fileName :: a -> Path Rel File
  size :: a -> Maybe FileOffset

instance FileOffer DccSend where
  fileName (Send p _ _ _) = fromPath p
  fileName (SendReverseServer p _ _ _) = fromPath p

  size (Send _ _ _ s) = s
  size (SendReverseServer _ _ s _) = Just s

data Env s = Env { remoteNick    :: !Nickname
                 , publicIP      :: !(Maybe IPv4)
                 , localPort     :: !(Maybe PortNumber)
                 , progressBar   :: Path Rel File -> Maybe FileOffset -> IO ProgressBar
                 , progress      :: ProgressBar -> FileOffset -> IO ()
                 , sendFn        :: IRC.UnicodeMessage -> IRC.StatefulIRC s ()
                 , putDccStateFn :: Status -> IRC.StatefulIRC s ()
                 , onDoneFn      :: IRC.StatefulIRC s ()
                 , onAbortFn     :: IRC.StatefulIRC s () }

data Status = Requesting
            | Downloading !DccSend
            | TryResuming !DccSend
            | Resuming !DccSend !FileOffset
            | Done
            | Aborting
    deriving (Eq, Show)

offerReceivedHandler :: IRC.UnicodeEvent -> DccIO s ()
offerReceivedHandler IRC.Event { _source  = IRC.User user
                               , _message = IRC.Privmsg _ (Left msg) } = do
    env <- getEnv
    when (user == remoteNick env) $
        either (const $ return ()) downloadOrTryResume (fromCtcp msg)
offerReceivedHandler _ = return ()

downloadOrTryResume :: DccSend -> DccIO s ()
downloadOrTryResume offer = do
    liftIO $ outputConcurrent
        ( "Received file offer for " ++ show (fileName offer)
       ++ maybe ", no file size provided.\n"
              (\s -> " of size " ++ show s ++ " bytes.\n") (size offer) )
    resumable <- liftIO $ isResumable (fileName offer) (size offer)
    case resumable of
      Right maybePos -> maybe (download FromStart offer)
                              (tryResume offer) maybePos
      Left err -> abort err

tryResume :: DccSend -> FileOffset -> DccIO s ()
tryResume offer pos = do
    rNick <- remoteNick <$> getEnv
    liftIO $ outputConcurrent ("Resumable file found with size " <> show pos <> ".\n")
    putDccState (TryResuming offer)
    sendCtcp rNick (resumeFromSend offer pos)

acceptResumeHandler :: DccSend -> IRC.UnicodeEvent -> DccIO s ()
acceptResumeHandler offer IRC.Event { _source  = IRC.User user
                                    , _message = IRC.Privmsg _ (Left msg) } = do
    env <- getEnv
    when (user == remoteNick env) $
        case fromCtcp msg of
          Right accept
              | accept `matchesSend` offer -> download (ResumeFrom (acceptedPosition accept)) offer
          _ -> return ()
acceptResumeHandler _ _ = return ()

download :: TransferType -> DccSend -> DccIO s ()
download transferType offer = do
    env <- getEnv
    case connectionType env offer of
      Right conType -> do
          liftIO $ outputConcurrent (msg transferType)
          putDccState (dlStatus transferType)
          downloadWithProgress (fileName offer) (size offer) conType transferType
          putDccState Done
          onDone
      Left err -> abort err
  where
    msg FromStart        = "No resumable file found, starting from zero...\n"
    msg (ResumeFrom pos) = "Resume from position " <> show pos <> "...\n"

    dlStatus FromStart        = Downloading offer
    dlStatus (ResumeFrom pos) = Resuming offer pos

connectionType :: Env s -> DccSend -> Either T.Text (ConnectionType (ReaderT (IRC.IRCState s) IO))
connectionType _ (Send _ ip port _) = Right $ Active ip port (return ())
connectionType env@Env {..} (SendReverseServer path' ip size' token) =
    Passive ip localPort . offerSocketReverse <$> publicIP'
  where
    offerSocketReverse pIP p = runDccIO env $
        sendCtcp remoteNick $ SendReverseClient path' pIP p size' token
    publicIP' = note ("Passive connections are only supported if you provide your external IP "
       <> "address on the command line using the '--public-ip' option. You could also try something "
       <> "like: '--public-ip `curl -s https://4.ifcfg.me`'.") publicIP

downloadWithProgress :: Path Rel File
                     -> Maybe FileOffset
                     -> ConnectionType (ReaderT (IRC.IRCState s) IO)
                     -> TransferType
                     -> DccIO s ()
downloadWithProgress name size' conType transType = do
    env  <- getEnv
    pBar <- liftIO $ progressBar env name size'
    liftIO $ progress env pBar (pos transType)
    transfer' Download
        { _fileName       = name
        , _connectionType = conType
        , _transferType   = transType
        , _onChunk        = liftIO . progress env pBar
        }
  where
    pos FromStart      = 0
    pos (ResumeFrom p) = p

abort :: T.Text -> DccIO s ()
abort err = do
    liftIO $ outputConcurrent (err <> "\n")
    putDccState Aborting
    onAbort

sendCtcp :: CtcpCommand a => Nickname -> a -> DccIO s ()
sendCtcp nick cmd = do
    liftIO $ outputConcurrent (show msg <> "\n")
    send msg
  where
    msg = IRC.Privmsg nick (Left $ toCtcp cmd)

isResumable :: Path Rel File -> Maybe FileOffset -> IO (Either T.Text (Maybe FileOffset))
isResumable file totalSize = do
    curSize <- getFileSizeSafe (fromRelFile file)
    case (curSize, totalSize) of
      (Just pos, Just total)
          | pos >= total -> return $ Left "File already exists and seems complete."
      (Just _, Nothing)  -> return $ Left "File already exists. Resuming not supported."
      (maybePos, _)      -> return $ Right maybePos

getFileSizeSafe :: FilePath -> IO (Maybe FileOffset)
getFileSizeSafe file = do
    exists <- fileExist file
    if exists
        then do
            stats <- getFileStatus file
            if isRegularFile stats
                then return $ Just (fromIntegral (Files.fileSize stats))
                else return Nothing
        else return Nothing
