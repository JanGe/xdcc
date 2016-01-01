{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc ( Connection
           , Network
           , Nickname
           , Channel
           , Pack
           , connectTo
           , disconnectFrom
           , onMessageDo
           , from
           , and
           , msgHasPrefix) where

import           Control.Concurrent
import           Control.Concurrent.Broadcast
import           Control.Monad
import           Data.ByteString.Char8        hiding (length, map, zip)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Prelude                      hiding (and)

import           Network.SimpleIRC

type Network = String
type Nickname = String
type Channel = CI String
type Pack = Int

type Connection = MIrc

config :: Network -> Nickname -> [(Channel, Broadcast ())] -> IrcConfig
config network name allChannelsJoined = (mkDefaultConfig network name) {
          cChannels = map (CI.original . fst) allChannelsJoined,
          cEvents   = map (\(channel, broadcast) ->
                              Join $ onJoin channel broadcast) allChannelsJoined
        }

connectTo :: Network -> Nickname -> [Channel] -> IO () -> IO () -> IO Connection
connectTo network nick chans onConnected onJoined =
  do channelsJoinedEvents <- replicateM (length chans) new
     let allChannelsJoined = zip chans channelsJoinedEvents
     let config' = config network nick allChannelsJoined
     connected <- connect config' True withDebug
     connection <- fromRight connected
     onConnected
     mapM_ listen $ snd <$> allChannelsJoined
     onJoined
     return connection
  where withDebug = False

disconnectFrom :: Connection -> IO ()
disconnectFrom = flip disconnect "bye"

onJoin :: Channel -> Broadcast () -> EventFunc
onJoin channel notifyJoined connection ircMessage =
  do curNick <- getNickname connection
     onMessageDo (for curNick `and` msgEqCi channel) (\_ ->
            broadcast notifyJoined ()) ircMessage

onMessageDo :: (a -> Bool) -> (a -> IO ()) -> a -> IO ()
onMessageDo predicate f x = when (predicate x) $ f x

for :: ByteString -> IrcMessage -> Bool
for nick IrcMessage { mNick = Just recipient } = nick == recipient
for _ _ = False

from :: ByteString -> IrcMessage -> Bool
from rNick IrcMessage { mOrigin = Just origin } = rNick == origin
from _ _ = False

msgEqCi :: CI String -> IrcMessage -> Bool
msgEqCi msg IrcMessage { mMsg } = msg == CI.mk (unpack mMsg)

msgHasPrefix :: ByteString -> IrcMessage -> Bool
msgHasPrefix prefix IrcMessage { mMsg } = prefix `isPrefixOf` mMsg

and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x = f x && g x

fromRight :: Either IOError a -> IO a
fromRight (Left e) = ioError e
fromRight (Right v) = return v
