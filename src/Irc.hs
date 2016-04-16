{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Irc ( IrcIO
           , IrcParams (..)
           , Connection
           , Network
           , Nickname
           , Password
           , Channel
           , Pack
           , EventFunc
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

import           IRC.Types

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listenTimeout, new)
import           Control.Error
import           Control.Monad                (replicateM, when, void)
import           Control.Monad.Trans.Class    (lift)
import           Data.ByteString.Char8        (ByteString, isPrefixOf, pack,
                                               unpack)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Monoid                  ((<>))
import           Network.IRC.CTCP             (CTCPByteString, asCTCP)
import           Prelude                      hiding (and)
import           System.Console.Concurrent    (outputConcurrent)
import           System.IO.Error              (ioeGetErrorString)
import           Control.Exception.Lifted

import           Network.SimpleIRC

connectTo :: Options -> IrcParams -> Bool -> IO () -> IO () -> IrcIO Connection
connectTo opt@Options{..} params withDebug onConnected onJoined = do
    (conf, bcs) <- lift $ config params
    con <- connect' conf withDebug
    lift $ mapM_ ((\f -> f con) . onConnect) (hooks params)
    lift onConnected
    joined <- lift $ catch (waitForAll bcs) (handler con)
    case sequence joined of
      Just _ -> do lift onJoined
                   return con
      Nothing -> throwE "Timeout when waiting on joining all channels."
  where
    handler :: Connection -> SomeException -> IO [Maybe ()]
    handler con ex = do
      outputConcurrent ("FAILURE xdcc: caught exception: " ++ show ex ++ "\n")
      void $ runExceptT $ disconnectFrom opt params con
      return ([Nothing])

disconnectFrom :: Options -> IrcParams -> Connection -> IrcIO ()
disconnectFrom Options{..} IrcParams { hooks } con = lift $ do
    mapM_ ((\f -> f con) . onDisconnect) hooks
    when (not zncAutoConnect) $ disconnect con "bye"

config :: IrcParams -> IO (IrcConfig, [Broadcast ()])
config IrcParams {..} = do
    bcs <- replicateM (length channels) Broadcast.new
    let conf = (mkDefaultConfig host nickname)
                 { cPort     = fromIntegral port
                 , cSecure   = secure
                 , cUsername = username
                 , cPass     = password
                 , cChannels = map CI.original channels
                 , cEvents   = concatMap onEvent hooks
                            ++ zipWith ((Join .) . onJoin) channels bcs }
    return (conf, bcs)

connect' :: IrcConfig -> Bool -> IrcIO Connection
connect' conf withDebug = do
    con <- lift $ hoistEither <$> connect conf True withDebug
    catchE con (throwE . ioeGetErrorString)

waitForAll :: [Broadcast ()] -> IO [Maybe ()]
waitForAll = mapM (`Broadcast.listenTimeout` 30000000)

sendAndWaitForAck :: Connection
                  -> Nickname
                  -> ByteString
                  -> (Nickname -> Broadcast b -> EventFunc)
                  -> String
                  -> IrcIO b
sendAndWaitForAck con rNick cmd broadCastIfMsg errMsg = do
    bc <- lift Broadcast.new
    v <- lift $ do changeEvents con
                                [ Privmsg (broadCastIfMsg rNick bc)
                                , Notice logMsg ]
                   send con rNick cmd
                   v <- Broadcast.listenTimeout bc 30000000
                   changeEvents con [ Notice logMsg ]
                   return v
    failWith errMsg v

send :: Connection -> Nickname -> ByteString -> IO ()
send con rNick msg = do
    sendCmd con command
    outputConcurrent (show command ++ "\n")
  where command = MPrivmsg (pack rNick) msg

onJoin :: Channel -> Broadcast () -> EventFunc
onJoin channel notifyJoined con ircMessage = do
    curNick <- getNickname con
    onMessage (for curNick `and` msgEqCi channel)
              (\_ -> broadcast notifyJoined ())
              ircMessage

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
