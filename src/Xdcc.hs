{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Xdcc ( requestFile
            , isResumable
            , Protocol (..)
            , FileMetadata (..)
            , fileMetadata
            , resumeFile
            , acceptFile
            ) where

import           Dcc
import           Irc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listenTimeout, new)
import           Control.Error
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Reader   (ask)
import           Control.Monad.Trans.Class    (lift)
import           Data.ByteString.Char8        (pack)
import qualified Data.ByteString.Lazy.Char8   as Lazy (fromStrict)
import           Data.Foldable                (traverse_)
import           Data.Monoid                  ((<>))
import           Network.IRC.CTCP             (asCTCP)

import           Network.SimpleIRC            (EventFunc,
                                               IrcEvent (Privmsg, Notice),
                                               IrcMessage (..), changeEvents,
                                               mMsg)

whenIsJust :: Monad m => Maybe a -> (a -> m b) -> m ()
whenIsJust value function = traverse_ function value

requestFile :: Pack -> (FileMetadata -> IO ()) -> IrcIO Protocol
requestFile num onReceive =
    do Context { connection, remoteNick } <- ask
       instructionsReceived <- liftIO Broadcast.new
       liftIO $ changeEvents connection
         [ Privmsg (onInstructionsReceived remoteNick instructionsReceived)
         , Notice logMsg ]
       liftIO $ sendMsgTo connection remoteNick message
       protocol <- liftIO (Broadcast.listenTimeout instructionsReceived 30000000)
       liftIO $ whenIsJust protocol (onReceive . fileMetadata)
       lift $ failWith "Didn't receive instructions in time." protocol
  where message = "XDCC SEND #" <> pack (show num)

onInstructionsReceived :: Nickname -> Broadcast Protocol -> EventFunc
onInstructionsReceived remoteNick instructionsReceived _ =
  onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
      case asCTCP mMsg >>= hush . parseDccProtocol of
         Just p -> broadcast instructionsReceived p
         Nothing -> return ())
