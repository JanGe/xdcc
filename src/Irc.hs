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
           , disconnectFrom
           , onMessageDo
           , from
           , and
           , msgHasPrefix
           , logMsg
           ) where

import           Control.Concurrent.Broadcast (Broadcast, broadcast, listen)
import qualified Control.Concurrent.Broadcast as Broadcast (new)
import           Control.Error
import           Control.Monad                (when)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Data.ByteString.Char8        hiding (length, map, zip)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.IP                      (IPv4)
import           Data.Monoid                  ((<>))
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
  do chansWithBroadcasts <- lift $ mapM attachBroadcast chans
     let config' = config network nick chansWithBroadcasts
     maybeConnected <- lift $ connect config' True withDebug
     connection <- hoistEither (catchEither maybeConnected
                                            (Left . ioeGetErrorString))
     lift onConnected
     lift $ waitForAll (map snd chansWithBroadcasts)
     lift onJoined
     return connection
  where attachBroadcast chan = do b <- Broadcast.new
                                  return (chan, b)
        waitForAll = mapM_ listen

disconnectFrom :: Connection -> IO ()
disconnectFrom = flip disconnect "bye"

onJoin :: Channel -> Broadcast () -> EventFunc
onJoin channel notifyJoined connection ircMessage =
  do curNick <- getNickname connection
     onMessageDo (for curNick `and` msgEqCi channel) (\_ ->
            broadcast notifyJoined ()) ircMessage

logMsg :: EventFunc
logMsg _ IrcMessage { mOrigin = Just origin, mCode, mMsg } =
    outputConcurrentBs (mCode <> " " <> origin <>": " <> mMsg <> "\n")
logMsg _ IrcMessage { mCode, mMsg } =
    outputConcurrentBs (mCode <> ": " <> mMsg <> "\n")

outputConcurrentBs :: ByteString -> IO ()
outputConcurrentBs = outputConcurrent . unpack

onMessageDo :: (a -> Bool) -> (a -> IO ()) -> a -> IO ()
onMessageDo predicate f x = when (predicate x) $ f x

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
