{-# LANGUAGE RecordWildCards #-}

module Main where

import           Irc
import           Xdcc

import           Control.Error
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask, asks, runReaderT)
import qualified Data.CaseInsensitive         as CI (mk)
import           Data.IP                      (IPv4)
import           Options.Applicative
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

options :: String -> Parser Options
options defaultNick = Options
    <$> strArgument ( metavar "HOST"
                    <> help "Host address of the IRC network" )
    <*> (CI.mk <$> strArgument ( metavar "CHANNEL"
                               <> help "Main channel to join on network" ))
    <*> strArgument ( metavar "USER"
                    <> help "Nickname of the user or bot to download from" )
    <*> argument auto ( metavar "#PACK"
                      <> help "Pack number of the file to download" )
    <*> strOption ( long "nickname"
                  <> short 'n'
                  <> metavar "NAME"
                  <> value defaultNick
                  <> help "Nickname to use for the IRC connection" )
    <*> many (CI.mk <$> strOption ( long "join"
                                  <> short 'j'
                                  <> metavar "CHANNEL" ))
    <*> optional ( option auto
                 ( long "publicIp"
                  <> short 'i'
                  <> metavar "IP"
                  <> help ("IPv4 address where you are reachable (only needed"
                          ++ " for Reverse DCC support).")))
    <*> switch ( long "verbose"
               <> short 'v'
               <> help "Enable verbose mode: verbosity level \"debug\"")

main :: IO ()
main = withConcurrentOutput $
       do defaultNick <- randomNick
          opts <- execParser (info (helper <*> options defaultNick) (
                      fullDesc
                      <> progDesc "Download files from IRC XDCC channels"
                      <> header "xdcc - an XDCC file downloader" ))
          result <- runExceptT $ runWith opts
          case result of
            Left e -> outputConcurrent ("FAILURE xdcc: " ++ e ++ "\n")
            Right _ -> return ()
  where randomNick = replicateM 10 $ randomRIO ('a', 'z')

runWith :: Options -> ExceptT String IO ()
runWith opts = withConnection opts $ withContext opts $
      runReaderT $ do protocol <- request (pack opts)
                      resumePos <- canResumeFrom protocol
                      case resumePos of
                        0 -> downloadWith protocol
                        pos -> resumeWith protocol pos

withConnection :: Options -> (Connection -> ExceptT String IO a)
                  -> ExceptT String IO a
withConnection Options {..} =
    bracket (connectAndJoin network nick channels verbose)
            (lift . disconnectFrom)
  where channels = mainChannel : additionalChannels

withContext :: Options -> (Context -> a) -> Connection -> a
withContext Options {..} f con = f Context { connection = con
                                           , publicIp = publicIp
                                           , remoteNick = rNick }

connectAndJoin :: Network -> Nickname -> [Channel] -> Bool
                  -> ExceptT String IO Connection
connectAndJoin network nick chans withDebug =
  do lift $ outputConcurrent ("Connecting to " ++ network ++ " as " ++ nick ++ "… ")
     connectTo network nick chans withDebug (
       outputConcurrent "Connected.\n") (
       outputConcurrent $ "Joined " ++ show chans ++ ".\n")

request :: Pack -> IrcIO Protocol
request pack =
  do rNick <- asks remoteNick
     liftIO $ outputConcurrent ("Requesting pack #" ++ show pack ++ " from "
                              ++ rNick ++ ", awaiting instructions…\n")
     requestFile pack (\f ->
       outputConcurrent ( "Received instructions for file " ++ show (fileName f)
                       ++ " of size " ++ show (fileSize f) ++ " bytes.\n" ))

downloadWith :: Protocol -> IrcIO ()
downloadWith p = do c <- ask
                    liftIO . displayConsoleRegions $ do
                       progressBar <- newDownloadBar (fileMetadata p)
                       acceptFile p c (tickN progressBar)

resumeWith :: Protocol -> Integer -> IrcIO ()
resumeWith p pos = do c <- ask
                      liftIO . displayConsoleRegions $ do
                         progressBar <- newDownloadBar (fileMetadata p)
                         tickN progressBar $ fromInteger pos
                         resumeFile p c (tickN progressBar) pos

newDownloadBar :: FileMetadata -> IO ProgressBar
newDownloadBar f =
      newProgressBar def { pgTotal = fileSize f
                         , pgWidth = 100
                         , pgFormat = barFormat
                         , pgOnCompletion = Just $ barFormat ++ " Done." }
  where barFormat = cap 30 (fileName f) ++ " [:bar] :percent (:current/:total)"

cap :: Int -> String -> String
cap maxLength string
    | length string > maxLength && maxLength > 1
                = take (maxLength - 1) string ++ "…"
    | otherwise = string

bracket :: (Monad m) =>
           ExceptT e m a -> (a -> ExceptT e m b) -> (a -> ExceptT e m c)
           -> ExceptT e m c
bracket acquire release apply = do
  r <- acquire
  z <- lift $ runExceptT $ apply r
  _ <- release r
  hoistEither z
