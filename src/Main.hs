{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified DCC
import           IRC.Types
import           XDCC
import qualified ZNC

import           Control.Concurrent.Async     (async, waitCatch)
import           Control.Exception            (SomeException)
import           Control.Monad                (replicateM)
import qualified Data.ByteString.Char8        as B (pack, unpack)
import qualified Data.CaseInsensitive         as CI (mk, original)
import           Data.IP                      (IPv4)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T (pack, unpack)
import qualified Network.IRC.Client           as IRC
import           Network.Socket               (PortNumber)
import           Options.Applicative.Extended
import           Path                         (File, Path, Rel, fromRelFile)
import           System.Console.AsciiProgress hiding (Options)
import           System.Console.Concurrent    (outputConcurrent,
                                               withConcurrentOutput)
import           System.Exit                  (exitFailure, exitSuccess)
import           System.Random                (randomRIO)

data Options = Options { network            :: !Network
                       , mainChannel        :: !Channel
                       , remoteNick         :: !Nickname
                       , packNumber         :: !Pack
                       , remotePort         :: !PortNumber
                       , useSecure          :: !Bool
                       , username           :: !Nickname
                       , password           :: !(Maybe Password)
                       , nickname           :: !Nickname
                       , additionalChannels :: ![Channel]
                       , publicIP           :: !(Maybe IPv4)
                       , localPort          :: !(Maybe PortNumber)
                       , zncAutoConnect     :: !Bool
                       , verbose            :: !Bool }
    deriving (Show)

options :: String -> ParserInfo Options
options defaultNick = info ( helper <*> opts )
                           ( fullDesc
                          <> header "xdcc - an XDCC file downloader"
                          <> progDesc ( "A wget-like utility for retrieving "
                                     ++ "files from XDCC bots on IRC" ))
  where opts = Options
          <$> ( B.pack <$> strArgument
              ( metavar "HOST"
             <> help "Host address of the IRC network" ))
          <*> ( CI.mk . T.pack <$> strArgument
              ( metavar "CHANNEL"
             <> help "Main channel to join on network" ))
          <*> ( T.pack <$> strArgument
              ( metavar "USER"
             <> help "Nickname of the user or bot to download from" ))
          <*> argument packNum
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
          <*> ( T.pack <$> strOption
              ( long "username"
             <> short 'u'
             <> metavar "USERNAME"
             <> value defaultNick
             <> help "Username used to authenticate on IRC network" ))
          <*> optional ( T.pack <$> strOption
              ( long "password"
             <> short 'p'
             <> metavar "PASSWORD"
             <> help "Password used to authenticate on IRC network" ))
          <*> ( T.pack <$> strOption
              ( long "nickname"
             <> short 'n'
             <> metavar "NICKNAME"
             <> value defaultNick
             <> help "Nickname to use for the IRC connection" ))
          <*> many ( CI.mk . T.pack <$> strOption
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
    opts        <- execParser $ options defaultNick
    result      <- mainWith opts
    case result of
      Left ex -> outputConcurrent ("FAILURE xdcc: " ++ show ex ++ "\n") >> exitFailure
      _       -> exitSuccess

  where
    randomNick = replicateM 10 $ randomRIO ('a', 'z')

-- TODO ZNC hooks
mainWith :: Options -> IO (Either SomeException ())
mainWith Options {..} = do
    hooks <- initHooks

    outputConcurrent ( "Connecting to " ++ B.unpack network ++ " on port "
                     ++ show remotePort ++ " as " ++ T.unpack nickname ++ "…\n" )
    conn <- connectWith logger network (fromIntegral remotePort) 1

    -- Only wait for joining main channel. It doesn't matter that we might not have
    -- joined all additional channels when initiating a file transfer.
    eventLoop <- async $ IRC.startStateful (connConfig conn hooks) (ircConfig hooks) initState
    waitCatch eventLoop
    -- TODO Handle errors

  where
    initState = XDCC.initialState mainChannel

    xdccConfig = XDCC.Env
        { packNumber = packNumber
        , dccEnv     = DCC.Env
            { remoteNick    = remoteNick
            , publicIP      = publicIP
            , localPort     = localPort
            , progressBar   = pBar
            , progress      = progressFn
            , sendFn        = IRC.send
            , putDccStateFn = XDCC.putDccState
            , onDoneFn      = IRC.disconnect
            , onAbortFn     = XDCC.onAbort remoteNick
            }
        }

    connConfig conn' hooks' = conn'
        { IRC._onconnect    = IRC.defaultOnConnect    >> mapM_ onConnect    hooks'
        , IRC._ondisconnect = IRC.defaultOnDisconnect >> mapM_ onDisconnect hooks'
        }

    ircConfig hooks' = (IRC.defaultIRCConf nickname)
        { IRC._username      = username
        , IRC._password      = password
        , IRC._channels      = CI.original <$> mainChannel : additionalChannels
        , IRC._eventHandlers = dispatcher xdccConfig
                             : mconcat (events <$> hooks')
                            <> IRC.defaultEventHandlers
        }

    initHooks = sequence [ZNC.autoConnectHooks | zncAutoConnect]

    connectWith
        | useSecure = IRC.connectWithTLS'
        | otherwise = IRC.connect'

    logger
        | verbose   = IRC.stdoutLogger
        | otherwise = IRC.noopLogger

progressFn :: Integral a => ProgressBar -> a -> IO ()
progressFn p = tickN p . fromIntegral

pBar :: Path Rel File -> Maybe DCC.FileOffset -> IO ProgressBar
pBar name size = newProgressBar $ def
    { pgTotal  = fromIntegral $ fromMaybe maxBound size
    , pgFormat = maybe formatUnknown (const format) size
    , pgWidth  = 100 }
  where
    cappedName    = cap 30 $ fromRelFile name
    formatUnknown = cappedName ++ " [:bar] (:current/unknown)"
    format        = cappedName ++ " [:bar] :percent (:current/:total)"

cap :: Int -> String -> String
cap bound s
  | length s > bound && bound > 1
              = take (bound - 1) s ++ "…"
  | otherwise = s
