{-# LANGUAGE RecordWildCards #-}

module Main where

import           Irc
import           Xdcc

import           Control.Error
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask, runReaderT)
import qualified Data.CaseInsensitive         as CI (mk)
import           Data.IP                      (IPv4)
import           Options.Applicative
import           Path                         (fromRelFile)
import           System.Console.AsciiProgress hiding (Options)
import           System.Console.Concurrent    (outputConcurrent,
                                               withConcurrentOutput)
import           System.Random                (randomRIO)

data Options = Options { network            :: Network
                       , mainChannel        :: Channel
                       , rNick              :: Nickname
                       , pack               :: Pack
                       , nick               :: Nickname
                       , additionalChannels :: [Channel]
                       , publicIp           :: Maybe IPv4
                       , verbose            :: Bool }
    deriving (Show)

options :: String -> ParserInfo Options
options defaultNick = info ( helper <*> opts )
                           ( fullDesc
                          <> header "xdcc - an XDCC file downloader"
                          <> progDesc ( "A wget-like utility for retrieving "
                                     ++ "files from XDCC bots on IRC" ))
  where opts = Options
          <$> strArgument
              ( metavar "HOST"
             <> help "Host address of the IRC network" )
          <*> ( CI.mk <$> strArgument
              ( metavar "CHANNEL"
             <> help "Main channel to join on network" ))
          <*> strArgument
              ( metavar "USER"
             <> help "Nickname of the user or bot to download from" )
          <*> argument auto
              ( metavar "#PACK"
             <> help "Pack number of the file to download" )
          <*> strOption
              ( long "nickname"
             <> short 'n'
             <> metavar "NAME"
             <> value defaultNick
             <> help "Nickname to use for the IRC connection" )
          <*> many ( CI.mk <$> strOption
              ( long "join"
             <> short 'j'
             <> metavar "CHANNEL" ))
          <*> optional ( option auto
              ( long "publicIp"
             <> short 'i'
             <> metavar "IP"
             <> help ( "IPv4 address where you are reachable (only needed for "
                    ++ "Reverse DCC support)." )))
          <*> switch
              ( long "verbose"
             <> short 'v'
             <> help "Enable verbose mode: verbosity level \"debug\"" )

main :: IO ()
main = withConcurrentOutput . displayConsoleRegions $
       do defaultNick <- randomNick
          opts <- execParser $ options defaultNick
          result <- runExceptT $ runWith opts
          case result of
            Left e -> outputConcurrent ("FAILURE xdcc: " ++ e ++ "\n")
            Right _ -> return ()

randomNick :: IO String
randomNick = replicateM 10 $ randomRIO ('a', 'z')

runWith :: Options -> ExceptT String IO ()
runWith opts = withIrcConnection opts . withDccEnv opts $
      runReaderT $ do o <- request (pack opts)
                      pos <- canResume o
                      case pos of
                        Just p -> resume o p
                        Nothing -> download o

withIrcConnection :: Options -> (Connection -> ExceptT String IO a)
                  -> ExceptT String IO a
withIrcConnection Options {..} =
    bracket (connectAndJoin network nick channels verbose)
            (lift . disconnectFrom)
  where channels = mainChannel : additionalChannels

withDccEnv :: Options -> (DccEnv -> a) -> Connection -> a
withDccEnv Options {..} f con = f DccEnv { connection = con
                                         , publicIp = publicIp
                                         , remoteNick = rNick }

connectAndJoin :: Network -> Nickname -> [Channel] -> Bool
                  -> ExceptT String IO Connection
connectAndJoin network nick chans withDebug = do
    lift $
        outputConcurrent $ "Connecting to " ++ network ++ " as " ++ nick ++ "… "
    connectTo network nick chans withDebug
        (outputConcurrent "Connected.\n")
        (outputConcurrent $ "Joined " ++ show chans ++ ".\n")

download :: OfferFile -> DccIO ()
download o@(OfferFile _ f) = do
    env <- ask
    lift $ withProgressBar f 0 $
        acceptFile o (offerSink env o)

resume :: OfferFile -> FileOffset -> DccIO ()
resume o@(OfferFile tt f) pos = do
    env <- ask
    lift $ withProgressBar f pos $
        resumeFile (AcceptResumeFile tt f pos) (offerSink env o)

withProgressBar :: FileMetadata
                -> FileOffset
                -> ((FileOffset -> IO ()) -> ExceptT String IO ())
                -> ExceptT String IO ()
withProgressBar file pos f = do
    progressBar <- liftIO $ newProgressBar opts
    liftIO $ tickN' progressBar pos
    f (tickN' progressBar)
  where opts = def
            { pgTotal = fromIntegral (fromMaybe maxBound (fileSize file))
            , pgWidth = 100
            , pgFormat = maybe formatUnknown (const format) (fileSize file) }
        cappedFn = cap 30 (fromRelFile (fileName file))
        format = cappedFn ++ " [:bar] :percent (:current/:total)"
        formatUnknown = cappedFn ++ " [:bar] (:current/unknown)"

tickN' :: Integral a => ProgressBar -> a -> IO ()
tickN' p = tickN p . fromIntegral

cap :: Int -> String -> String
cap bound s
  | length s > bound && bound > 1
              = take (bound - 1) s ++ "…"
  | otherwise = s

bracket :: (Monad m) =>
           ExceptT e m a -> (a -> ExceptT e m b) -> (a -> ExceptT e m c)
           -> ExceptT e m c
bracket acquire release apply = do
    r <- acquire
    z <- lift $ runExceptT $ apply r
    _ <- release r
    hoistEither z
