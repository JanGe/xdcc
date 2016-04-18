{-# LANGUAGE RecordWildCards #-}

module Main where

import           Irc
import           Xdcc
import qualified Znc
import           IRC.Types

import           Control.Error
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask, runReaderT)
import qualified Data.CaseInsensitive         as CI (mk)
import           Options.Applicative.Extended
import           Path                         (fromRelFile)
import           System.Console.AsciiProgress hiding (Options)
import           System.Console.Concurrent    (outputConcurrent,
                                               withConcurrentOutput)
import           System.Random                (randomRIO)
import           Control.Exception.Lifted     (bracket)

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
          <*> option tcpPort
              ( long "port"
             <> short 'P'
             <> metavar "PORT"
             <> value 6667
             <> help "Port the IRC network is available on (default is 6667)" )
          <*> switch
              ( long "secure"
             <> short 's'
             <> help "Use a secure transport (TLS) for the IRC connection" )
          <*> strOption
              ( long "username"
             <> short 'u'
             <> metavar "USERNAME"
             <> value defaultNick
             <> help "Username used to authenticate on IRC network" )
          <*> optional ( strOption
              ( long "password"
             <> short 'p'
             <> metavar "PASSWORD"
             <> help "Username used to authenticate on IRC network" ))
          <*> strOption
              ( long "nickname"
             <> short 'n'
             <> metavar "NICKNAME"
             <> value defaultNick
             <> help "Nickname to use for the IRC connection" )
          <*> many ( CI.mk <$> strOption
              ( long "join"
             <> short 'j'
             <> metavar "CHANNEL"
             <> help "Join additional channels" ))
          <*> optional ( option auto
              ( long "public-ip"
             <> short 'i'
             <> metavar "IP"
             <> help ( "IPv4 address where you are reachable from the outside "
                    ++ "(only needed for Reverse DCC support)" )))
          <*> optional ( option tcpPort
              ( long "bind-port"
             <> short 'b'
             <> metavar "PORT"
             <> help ( "Local port to bind to (default is an arbitrary port "
                    ++ "selected by the operating system; only needed for "
                    ++ "Reverse DCC support)" )))
          <*> switch
              ( long "znc-auto-connect"
             <> short 'z'
             <> help ( "Tell ZNC IRC bouncer to connect to IRC network if "
                    ++ "disconnected" ))
          <*> switch
              ( long "verbose"
             <> short 'v'
             <> help "Enable verbose mode: verbosity level \"debug\"" )

main :: IO ()
main = withConcurrentOutput . displayConsoleRegions $ do
         defaultNick <- randomNick
         opts <- execParser $ options defaultNick
         result <- runExceptT $ runWith opts
         case result of
           Left e -> outputConcurrent ("FAILURE xdcc: " ++ e ++ "\n")
           Right _ -> return ()

randomNick :: IO String
randomNick = replicateM 10 $ randomRIO ('a', 'z')

runWith :: Options -> ExceptT String IO ()
runWith opts = withIrcConnection opts . withDccEnv opts $
    runReaderT $ do o <- request (packno opts)
                    pos <- canResume o
                    case pos of
                      Just p -> resume o p
                      Nothing -> download o

withIrcConnection :: Options -> (Connection -> ExceptT String IO a)
                  -> ExceptT String IO a
withIrcConnection opt@Options {..} =
    bracket (connectAndJoin opt params verbose)
            (disconnectFrom opt params)
  where params = IrcParams { host     = network
                           , port     = rPort
                           , secure   = usesecure
                           , username = user
                           , password = pass
                           , nickname = nick
                           , channels = mainChannel : additionalChannels
                           , hooks = [Znc.autoConnectHooks | zncAutoConnect] }

withDccEnv :: Options -> (DccEnv -> a) -> Connection -> a
withDccEnv Options {..} f con = f DccEnv { connection = con
                                         , publicIp = publicIp
                                         , remoteNick = rNick
                                         , localPort = lPort }

connectAndJoin :: Options -> IrcParams -> Bool -> ExceptT String IO Connection
connectAndJoin opt@Options{..} params withDebug = do
    lift $ outputConcurrent
        ( "Connecting to " ++ host params ++ " on port " ++ show (port params)
       ++ " as " ++ nickname params ++ "… " )
    connectTo opt params withDebug
        (outputConcurrent "Connected.\n")
        (outputConcurrent ("Joined " ++ show (channels params) ++ ".\n"))

download :: OfferFile -> DccIO ()
download o@(OfferFile _ f) = do
    env <- ask
    lift $ withProgressBar f 0 (\onChunk ->
        runReaderT
            (acceptFile o (offerSink env o) onChunk)
            (localPort env) )

resume :: OfferFile -> FileOffset -> DccIO ()
resume o@(OfferFile tt f) pos = do
    env <- ask
    lift $ withProgressBar f pos (\onChunk ->
        runReaderT
            (resumeFile (AcceptResumeFile tt f pos) (offerSink env o) onChunk)
            (localPort env) )

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

