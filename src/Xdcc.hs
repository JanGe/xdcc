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
import           Network.IRC.CTCP             (decodeCTCP, asCTCP)
import           Text.Parse.ByteString        (onFail, runParser)

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
    do receivedInstructions <- lift Broadcast.new
       lift $ changeEvents con
           [ Privmsg (parseInstructions (remoteNick c) receivedInstructions)
           , Notice logMsg ]
       lift $ sendMsgTo con (remoteNick c) message
       instructions <- lift $ Broadcast.listenTimeout receivedInstructions 30000000
       lift $ whenIsJust instructions (onReceive . fileMetadata)
       failWith "Didn't receive instructions in time." instructions
  where message = "XDCC SEND #" <> pack (show num)

parseInstructions :: Nickname -> Broadcast Protocol -> EventFunc
parseInstructions remoteNick instructionsReceived _ =
  onMessageDo (from remoteNick) (\IrcMessage { mMsg } ->
      let msg = asCTCP mMsg in
      case msg of
        Just m -> case runParser parser (Lazy.fromStrict (decodeCTCP m)) of
           (Right protocol, _) -> broadcast instructionsReceived protocol
           (Left e, _) -> putStrLn e
        Nothing -> return ())
  where parser = offerParserDcc `onFail` offerParserReverseDcc
