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
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Reader   (ask, asks)
import           Data.ByteString.Char8        (pack)
import           Data.Foldable                (traverse_)
import           Data.Monoid                  ((<>))
import           System.Console.Concurrent    (outputConcurrent)

import           Network.SimpleIRC            (EventFunc)

request :: Pack -> IrcIO Offer
request p =
  do rNick <- asks remoteNick
     liftIO $ outputConcurrent ("Requesting pack #" ++ show p ++ " from "
                              ++ rNick ++ ", awaiting instructions…\n")
     o <- requestFile p
     let FileMetadata fn fs = fileMetadata o
     liftIO $ outputConcurrent ( "Received instructions for file " ++ show fn
                              ++ " of size " ++ show fs ++ " bytes.\n" )
     return o

-- TODO XDCC CANCEL on failure
requestFile :: Pack -> IrcIO Offer
requestFile num =
    do Context { remoteNick } <- ask
       sendAndWaitForAck ("XDCC SEND #" <> pack (show num))
                         (onInstructionsReceived remoteNick)
                         "Timeout when waiting for file offer."

onInstructionsReceived :: Nickname -> Broadcast Offer -> EventFunc
onInstructionsReceived rNick bc _ =
  onCtcpMessage (from rNick)
                (traverse_ (broadcast bc) . runParser decodeOffer)
