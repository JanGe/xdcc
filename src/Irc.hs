{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc ( IrcIO
           , Context (..)
           , Connection
           , Network
           , Nickname
           , Channel
           , Pack
           , connectTo
           , sendAndWaitForAck
           , send
           , disconnectFrom
           , onMessage
           , onCtcpMessage
           , from
           , and
           , msgHasPrefix
           , logMsg
           ) where

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listenTimeout, new)
import           Control.Error
import           Control.Monad                (replicateM, when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT, ask)
import           Data.ByteString.Char8        hiding (length, map, zip)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.IP                      (IPv4)
import           Data.Monoid                  ((<>))
import           Network.IRC.CTCP             (CTCPByteString, asCTCP)
import           Prelude                      hiding (and)
import           System.Console.Concurrent    (outputConcurrent)
import           System.IO.Error              (ioeGetErrorString)

import           Network.SimpleIRC

type Network = String
type Nickname = String
type Channel = CI String
type Pack = Int

type Connection = MIrc

type IrcIO a = ReaderT Context (ExceptT String IO) a

data Context = Context { connection :: Connection
                       , remoteNick :: Nickname
                       , publicIp   :: Maybe IPv4 }

config :: Network -> Nickname -> [(Channel, Broadcast ())] -> IrcConfig
config network nick chansWithBroadcasts = (mkDefaultConfig network nick)
    { cChannels = map (CI.original . fst) chansWithBroadcasts
    , cEvents   = map asOnJoinEvent chansWithBroadcasts }
  where asOnJoinEvent (chan, b) = Join (onJoin chan b)

connectTo :: Network -> Nickname -> [Channel] -> Bool -> IO () -> IO ()
             -> ExceptT String IO Connection
connectTo network nick chans withDebug onConnected onJoined =
  do bcs <- lift $ replicateM (length chans) Broadcast.new
     conMaybe <- lift $ connect (config' bcs) True withDebug
     con <- hoistEither (catchEither conMaybe (Left . ioeGetErrorString))
     lift onConnected
     joined <- lift $ waitForAll bcs
     case sequence joined of
       Just _ -> do lift onJoined
                    return con
       Nothing -> throwE "Timeout when waiting on joining all channels."
  where config' bcs = config network nick (chans `zip` bcs)

waitForAll :: [Broadcast ()] -> IO [Maybe ()]
waitForAll = mapM (`Broadcast.listenTimeout` 30000000)

sendAndWaitForAck :: ByteString -> (Broadcast b -> EventFunc) -> String
                  -> IrcIO b
sendAndWaitForAck cmd broadCastIfMsg errMsg =
    do c <- ask
       bc <- liftIO Broadcast.new
       v <- liftIO $ do changeEvents (connection c)
                                     [ Privmsg (broadCastIfMsg bc)
                                     , Notice logMsg ]
                        send c cmd
                        Broadcast.listenTimeout bc 30000000
       lift $ failWith errMsg v

send :: Context -> ByteString -> IO ()
send Context { connection, remoteNick } msg =
    do sendCmd connection command
       outputConcurrent (show command ++ "\n")
  where command = MPrivmsg (pack remoteNick) msg

disconnectFrom :: Connection -> IO ()
disconnectFrom = flip disconnect "bye"

onJoin :: Channel -> Broadcast () -> EventFunc
onJoin channel notifyJoined con ircMessage =
  do curNick <- getNickname con
     onMessage (for curNick `and` msgEqCi channel) (\_ ->
            broadcast notifyJoined ()) ircMessage

logMsg :: EventFunc
logMsg _ IrcMessage { mOrigin = Just origin, mCode, mMsg } =
    outputConcurrentBs (mCode <> " " <> origin <>": " <> mMsg <> "\n")
logMsg _ IrcMessage { mCode, mMsg } =
    outputConcurrentBs (mCode <> ": " <> mMsg <> "\n")

outputConcurrentBs :: ByteString -> IO ()
outputConcurrentBs = outputConcurrent . unpack

onMessage :: (a -> Bool)
          -> (a -> IO ())
          -> a
          -> IO ()
onMessage p f x = when (p x) $ f x

onCtcpMessage :: (IrcMessage -> Bool)
              -> (CTCPByteString -> IO ())
              -> IrcMessage
              -> IO ()
onCtcpMessage p f = onMessage p (sequence_ . fmap f . asCTCP . mMsg)

for :: ByteString -> IrcMessage -> Bool
for nick IrcMessage { mNick = Just recipient } = nick == recipient
for _ _ = False

from :: Nickname -> IrcMessage -> Bool
from rNick IrcMessage { mOrigin = Just origin } = pack rNick == origin
from _ _ = False

msgEqCi :: CI String -> IrcMessage -> Bool
msgEqCi msg IrcMessage { mMsg } = msg == CI.mk (unpack mMsg)

msgHasPrefix :: ByteString -> IrcMessage -> Bool
msgHasPrefix prefix IrcMessage { mMsg } = prefix `isPrefixOf` mMsg

and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x = f x && g x
