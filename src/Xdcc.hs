{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Xdcc ( requestFile
            , isResumable
            , Protocol (..)
            , Context (..)
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

whenIsJust :: Maybe a -> (a -> IO b) -> IO ()
whenIsJust value function = traverse_ function value

requestFile :: Connection -> Context -> Pack ->
               (FileMetadata -> IO ())
               -> ExceptT String IO Protocol
requestFile con c num onReceive =
    do instructionsReceived <- lift Broadcast.new
       lift $ changeEvents con
         [ Privmsg (onInstructionsReceived (remoteNick c) instructionsReceived)
         , Notice logMsg ]
       lift $ sendMsgTo con (remoteNick c) message
       protocol <- lift $ Broadcast.listenTimeout instructionsReceived 30000000
       lift $ whenIsJust protocol (onReceive . fileMetadata)
       failWith "Didn't receive instructions in time." protocol
  where message = "XDCC SEND #" <> pack (show num)

onInstructionsReceived :: Nickname -> Broadcast Protocol -> EventFunc
onInstructionsReceived remoteNick instructionsReceived _ =
  onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
      case asCTCP mMsg >>= hush . parseDccProtocol of
         Just p -> broadcast instructionsReceived p
         Nothing -> return ())
