{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Broadcast
import qualified Control.Exception            as E
import           Control.Monad
import           Data.Binary
import           Data.Binary.Put
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.IP
import           Data.Maybe
import           Data.Time
import           Data.Word
import           Debug.Trace
import qualified Network.Socket               as S hiding (recv, recvFrom, send,
                                                    sendTo)
import qualified Network.Socket.ByteString    as S
import qualified Options.Applicative          as Opts
import           System.FilePath
import           System.IO
import qualified System.IO.Streams            as IOS
import           System.Random
import           Text.Parse.ByteString
import           Text.Printf

import           Network.SimpleIRC
import           System.Console.AsciiProgress hiding (Options)

type Network = String
type Nickname = String
type Channel = CI String
type Pack = Int

data Options = Options { network            :: Network
                       , mainChannel        :: Channel
                       , remoteNick         :: Nickname
                       , pack               :: Pack
                       , nick               :: Nickname
                       , additionalChannels :: [Channel] }
                  deriving (Show)

data SendOffer = SendOffer { host     :: S.HostAddress
                           , port     :: S.PortNumber
                           , fileName :: FilePath
                           , fileSize :: Maybe Integer }
                 deriving (Show)

notify :: Eq a => [(a, Broadcast ())] -> a -> IO ()
notify broadcasts key =
    case lookup key broadcasts of
        Just b -> broadcast b ()
        _ -> return ()

onJoin :: (Channel -> IO ()) -> EventFunc
onJoin notifyJoined connection IrcMessage { mNick, mMsg = channel } =
    do nick <- getNickname connection
       case mNick of
         Just mNick | mNick == nick -> notifyJoined $ (CI.mk . B.unpack) channel
         _ -> return ()

onDccSendFrom :: B.ByteString -> Broadcast SendOffer -> EventFunc
onDccSendFrom rNick dccSendBroadcast _ IrcMessage { mMsg = msg, mOrigin } =
    case mOrigin of
        Just origin | origin == rNick
                      && "\SOHDCC SEND " `B.isPrefixOf` msg
                         -> case parseSendOffer msg of
                              Left e -> ioError $ userError e
                              Right offer -> broadcast dccSendBroadcast offer
        _ -> return ()

parseSendOffer :: B.ByteString -> Either String SendOffer
parseSendOffer msg = case result of
        (Left error, unconsumed) -> Left ("Encountered error: " ++ error ++
                                          " when parsing: " ++ BL.unpack unconsumed)
        (Right offer, _) -> Right offer
    where parser = do literal "\SOHDCC SEND "
                      fileName <- many1Satisfy (/= ' ')
                      literal " "
                      ip <- parseUnsignedInteger
                      literal " "
                      port <- parseUnsignedInteger
                      fileSize <- fmap Just (do literal " "
                                                parseUnsignedInteger)
                                             `onFail` return Nothing
                      return SendOffer {
                          host = byteSwap32 $ fromIntegral ip,
                          port = fromInteger port,
                          fileName = takeFileName $ BL.unpack fileName,
                          fileSize = fileSize
                      }
          result = runParser parser $ BL.fromStrict msg

config :: Network -> Nickname -> [(Channel, Broadcast ())] -> IrcConfig
config network name allChannelsJoined =
    let notifyJoined = notify allChannelsJoined in
        (mkDefaultConfig network name) {
          cChannels = map (CI.original . fst) allChannelsJoined,
          cEvents   = [ Join $ onJoin notifyJoined ]
        }

connectAndJoin :: Network -> Nickname -> [Channel] -> IO MIrc
connectAndJoin network name chans =
        do channelsJoinedEvents <- replicateM (length chans) new
           let allChannelsJoined = zip chans channelsJoinedEvents
           putStr $ "Connecting to " ++ network ++ "... "
           let config' = config network name allChannelsJoined
           connected <- connect config' True False
           putStrLn "connected."
           connection <- fromRight connected
           putStr $ "Joining channels " ++ show chans ++ "... "
           let events = map snd allChannelsJoined
           mapM_ listen events
           putStrLn "joined."
           return connection

requestFile :: MIrc -> Nickname -> Pack -> IO SendOffer
requestFile connection remoteNick pack =
  let rNick = B.pack remoteNick in
  do receivedSendOffer <- new
     changeEvents connection [ Privmsg $ onDccSendFrom rNick receivedSendOffer ]
     let sendMessage = MPrivmsg rNick $ B.pack $ "xdcc send #" ++ show pack
     sendCmd connection sendMessage
     putStr $ "" ++ show sendMessage ++ " sent, awaiting instructions... "
     sendOffer <- listen receivedSendOffer
     putStrLn $ "received instructions for file " ++ show (fileName sendOffer)
     return sendOffer

download :: SendOffer -> IOS.OutputStream B.ByteString -> IO ()
download SendOffer { host, port, fileName, fileSize } file =
  displayConsoleRegions . S.withSocketsDo $
    do ip <- S.inet_ntoa host
       putStr $ "Connecting to " ++ ip ++ ":" ++ show port ++ "... "
       sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
       S.connect sock $ S.SockAddrInet port host
       putStrLn "connected."
       progressBar <- newDownloadBar fileSize fileName
       downloadToFile sock file progressBar 0
       S.sClose sock


newDownloadBar fileSize fileName =
      newProgressBar def { pgTotal = fromMaybe 1 fileSize
                         , pgWidth = 100
                         , pgFormat = barFormat
                         , pgOnCompletion = Just $ barFormat ++ " Done." }
  where barFormat = cap 30 fileName ++ " [:bar] :percent (:current/:total)"

cap :: Int -> String -> String
cap maxLength string | length string > maxLength
                       = take (maxLength - 3) string ++ "..."
cap _ string           = string

downloadToFile :: S.Socket -> IOS.OutputStream B.ByteString -> ProgressBar -> Int -> IO ()
downloadToFile sock file progressBar size =
   do buffer <- S.recv sock $ 4096 * 1024
      let received = B.length buffer
      let receivedTotal = size + received
      if B.null buffer
          then do complete progressBar
                  return ()
          else do tickN progressBar received
                  sendNumReceived sock receivedTotal
                  Just buffer `IOS.write` file
                  downloadToFile sock file progressBar receivedTotal

toNetworkByteOrder :: Int -> B.ByteString
toNetworkByteOrder = BL.toStrict . runPut . putWord32be . fromIntegral

sendNumReceived :: S.Socket -> Int -> IO ()
sendNumReceived sock num = S.sendAll sock (toNetworkByteOrder num)

fromRight :: Either IOError a -> IO a
fromRight (Left e) = ioError e
fromRight (Right v) = return v

run :: Options -> IO ()
run (Options network mainChan remoteNick pack nick moreChans) =
    do connection <- connectAndJoin network nick $ mainChan : moreChans
       sendOffer <- requestFile connection remoteNick pack
       IOS.withFileAsOutput (fileName sendOffer) $ download sendOffer
      --  let cancelMessage = MPrivmsg (B.pack remoteNick) "xdcc cancel"
      --  B.putStrLn $ showCommand cancelMessage
      --  sendCmd connection cancelMessage
       disconnect connection "bye"

randomNick :: IO Nickname
randomNick = replicateM 10 randomChar
    where randomChar = randomRIO ('a', 'z')

options :: String -> Opts.Parser Options
options defaultNick = Options
    <$> Opts.strArgument ( Opts.metavar "HOST"
                         Opts.<> Opts.help "Host address of the IRC network" )
    <*> fmap CI.mk (Opts.strArgument ( Opts.metavar "CHANNEL"
                                     Opts.<> Opts.help "Main channel to join on network" ))
    <*> Opts.strArgument ( Opts.metavar "USER"
                         Opts.<> Opts.help "Nickname of the user or bot to download from" )
    <*> Opts.argument Opts.auto ( Opts.metavar "#PACK"
                         Opts.<> Opts.help "Pack number of the file to download" )
    <*> Opts.strOption ( Opts.long "nickname"
                       Opts.<> Opts.short 'n'
                       Opts.<> Opts.metavar "NAME"
                       Opts.<> Opts.value defaultNick
                       Opts.<> Opts.help "")
    <*> Opts.many (fmap CI.mk (Opts.strOption ( Opts.long "join"
                                              Opts.<> Opts.short 'j'
                                              Opts.<> Opts.metavar "CHANNEL" )))

main :: IO ()
main = do defaultNick <- randomNick
          Opts.execParser (
              Opts.info (Opts.helper <*> options defaultNick) (
                  Opts.fullDesc
                  Opts.<> Opts.progDesc "Download files from IRC XDCC channels"
                  Opts.<> Opts.header "xdcc - an XDCC file downloader" ))
          >>= run
