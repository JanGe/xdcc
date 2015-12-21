
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Irc
import           Xdcc

import           Control.Monad                (replicateM)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Maybe                   (fromMaybe)
import           Options.Applicative
import           System.Random                (randomRIO)

import           System.Console.AsciiProgress hiding (Options)

data Options = Options { network            :: Network
                       , mainChannel        :: Channel
                       , remoteNick         :: Nickname
                       , pack               :: Pack
                       , nick               :: Nickname
                       , additionalChannels :: [Channel] }
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
                  <> help "" )
    <*> many (CI.mk <$> strOption ( long "join"
                                  <> short 'j'
                                  <> metavar "CHANNEL" ))

main :: IO ()
main = do defaultNick <- randomNick
          execParser (info (helper <*> options defaultNick) (
            fullDesc
            <> progDesc "Download files from IRC XDCC channels"
            <> header "xdcc - an XDCC file downloader" ))
          >>= run
  where randomNick = replicateM 10 $ randomRIO ('a', 'z')

run :: Options -> IO ()
run Options {..} = do let channels = mainChannel : additionalChannels
                      connection <- connectAndJoin network nick channels
                      instructions <- requestFile connection remoteNick pack
                      downloadWith instructions
                      disconnectFrom connection

connectAndJoin :: Network -> Nickname -> [Channel] -> IO Connection
connectAndJoin network nick chans =
  do putStr $ "Connecting to " ++ network ++ " as " ++ nick ++ "... "
     putStr $ "Joining channels " ++ show chans ++ "... "
     connectTo network nick chans (
       putStrLn "connected.") (
       putStrLn "joined.")

requestFile :: Connection -> Nickname -> Pack -> IO Instructions
requestFile connection remoteNick pack =
  do putStr $ "Requesting pack #" ++ show pack
     putStr $ " from "  ++ remoteNick ++ ", awaiting instructions... "
     sendFileRequest connection remoteNick pack (\instructions ->
      putStrLn $ "received instructions for file " ++ fileName instructions)

downloadWith :: Instructions -> IO ()
downloadWith instructions@Instructions {..} =
  displayConsoleRegions $
    do putStr $ "Connecting to " ++ show host ++ ":" ++ show port ++ "... "
       progressBar <- newDownloadBar fileSize fileName
       acceptFile instructions (putStrLn "connected.") (tickN progressBar)

newDownloadBar :: Maybe Integer -> String -> IO ProgressBar
newDownloadBar fileSize fileName =
      newProgressBar def { pgTotal = fromMaybe 1 fileSize
                         , pgWidth = 100
                         , pgFormat = barFormat
                         , pgOnCompletion = Just $ barFormat ++ " Done." }
  where barFormat = cap 30 fileName ++ " [:bar] :percent (:current/:total)"

cap :: Int -> String -> String
cap maxLength string | length string > maxLength && maxLength > 1
                       = take (maxLength - 1) string ++ "…"
cap _         string   = string
