{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Xdcc ( requestFile
            , request
            , canResume
            , FileMetadata
            , fileName
            , fileSize
            , FileOffset
            , Offer (..)
            , AcceptResume (..)
            , resumeFile
            , acceptFile
            , offerSink
            ) where

import           Dcc
import           Irc

import           Network.IRC.DCC

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast (listenTimeout, new)
import           Control.Error
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask, asks)
import           Data.ByteString.Char8        (pack)
import           Data.Foldable                (traverse_)
import           System.Console.Concurrent    (outputConcurrent)
import           Data.Monoid                  ((<>))

import           Network.SimpleIRC            (EventFunc,
                                               IrcEvent (Privmsg, Notice),
                                               IrcMessage (..), changeEvents,
                                               mMsg)

whenIsJust :: Monad m => Maybe a -> (a -> m b) -> m ()
whenIsJust value function = traverse_ function value

-- TODO XDCC CANCEL on failure
requestFile :: Pack -> (FileMetadata -> IO ()) -> IrcIO Offer
requestFile num onReceive =
    do c <- ask
       instructionsReceived <- liftIO Broadcast.new
       liftIO $ changeEvents (connection c)
         [ Privmsg (onInstructionsReceived (remoteNick c) instructionsReceived)
         , Notice logMsg ]
       liftIO $ sendMsg c message
       offer <- liftIO (Broadcast.listenTimeout instructionsReceived 30000000)
       liftIO $ whenIsJust offer (onReceive . fileMetadata)
       lift $ failWith "Didn't receive instructions in time." offer
  where message = "XDCC SEND #" <> pack (show num)

onInstructionsReceived :: Nickname -> Broadcast Offer -> EventFunc
onInstructionsReceived remoteNick instructionsReceived _ =
  onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
      case doIfCtcp (runParser decodeOffer) mMsg of
         Right p -> broadcast instructionsReceived p
         _ -> return ())

request :: Pack -> IrcIO Offer
request p =
  do rNick <- asks remoteNick
     liftIO $ outputConcurrent ("Requesting pack #" ++ show p ++ " from "
                              ++ rNick ++ ", awaiting instructions…\n")
     requestFile p (\(FileMetadata fn fs) ->
       outputConcurrent ( "Received instructions for file " ++ show fn
                       ++ " of size " ++ show fs ++ " bytes.\n" ))
