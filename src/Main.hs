
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Irc
import           Xdcc

import           Control.Monad                (replicateM)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Maybe                   (fromMaybe)
import           Network.Socket               (HostAddress, HostName,
                                               SockAddr (SockAddrInet),
                                               addrAddress, getAddrInfo,
                                               inet_ntoa)
import           Options.Applicative
import           System.IO                    (BufferMode (NoBuffering),
                                               hSetBuffering, stdout)
import           System.Random                (randomRIO)

import           System.Console.AsciiProgress hiding (Options)

data Options = Options { network            :: Network
                       , mainChannel        :: Channel
                       , remoteNick         :: Nickname
                       , pack               :: Pack
                       , nick               :: Nickname
                       , additionalChannels :: [Channel]
                       , publicIp           :: HostName }
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
    <*> strOption ( long "publicIp"
                  <> short 'i'
                  <> metavar "IP"
                  <> help ("IP address where you are reachable (only needed for"
                          ++ " Reverse DCC support)."))

main :: IO ()
main = do defaultNick <- randomNick
          execParser (info (helper <*> options defaultNick) (
            fullDesc
            <> progDesc "Download files from IRC XDCC channels"
            <> header "xdcc - an XDCC file downloader" ))
          >>= run
  where randomNick = replicateM 10 $ randomRIO ('a', 'z')

run :: Options -> IO ()
run Options {..} = do hSetBuffering stdout NoBuffering
                      let channels = mainChannel : additionalChannels
                      connection <- connectAndJoin network nick channels
                      instructions <- requestFile connection publicIp remoteNick pack
                      downloadWith connection instructions
                      disconnectFrom connection

connectAndJoin :: Network -> Nickname -> [Channel] -> IO Connection
connectAndJoin network nick chans =
  do putStr $ "Connecting to " ++ network ++ " as " ++ nick ++ "… "
     connectTo network nick chans (
       putStrLn "Connected.") (
       putStrLn $ "Joined " ++ show chans ++ ".")

requestFile :: Connection -> HostName -> Nickname -> Pack -> IO Parameters
requestFile connection publicIp remoteNick pack =
  do addrInfo <- getAddrInfo Nothing (Just publicIp) Nothing
     let SockAddrInet _ ip = addrAddress $ head addrInfo
     putStrLn $ "Requesting pack #" ++ show pack ++
              " from "  ++ remoteNick ++ ", awaiting instructions…"
     sendFileRequest connection ip remoteNick pack (\instructions ->
       putStrLn $ "Received instructions for file " ++
                  fileName instructions ++ ".")

downloadWith :: Connection -> Parameters -> IO ()
downloadWith connection parameters@DccSend {..} = displayConsoleRegions $
  do ip <- inet_ntoa host
     putStrLn $ "Connecting to " ++ ip ++ ":" ++ show port ++ "…"
     progressBar <- newDownloadBar fileSize fileName
     acceptFile connection parameters (tickN progressBar)
downloadWith connection parameters@ReverseDcc {..} = displayConsoleRegions $
  do ip <- inet_ntoa host
     putStrLn $ "Awaiting connection from " ++ ip ++ "…"
     progressBar <- newDownloadBar fileSize fileName
     acceptFile connection parameters (tickN progressBar)

newDownloadBar :: Integer -> String -> IO ProgressBar
newDownloadBar fileSize fileName =
      newProgressBar def { pgTotal = fileSize
                         , pgWidth = 100
                         , pgFormat = barFormat
                         , pgOnCompletion = Just $ barFormat ++ " Done." }
  where barFormat = cap 30 fileName ++ " [:bar] :percent (:current/:total)"

cap :: Int -> String -> String
cap maxLength string | length string > maxLength && maxLength > 1
                       = take (maxLength - 1) string ++ "…"
cap _         string   = string
