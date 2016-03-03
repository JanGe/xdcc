{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Xdcc ( OfferFile(..)
            , AcceptResumeFile(..)
            , FileMetadata(..)
            , FileOffset
            , request
            , acceptFile
            , canResume
            , resumeFile
            , offerSink
            ) where

import           Dcc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Reader   (ask, asks)
import           Data.ByteString.Char8        (pack)
import           Data.Monoid                  ((<>))
import           Network.IRC.CTCP             (encodeCTCP)
import           System.Console.Concurrent    (outputConcurrent)

request :: Pack -> IrcIO OfferFile
request p =
  do rNick <- asks remoteNick
     liftIO $ outputConcurrent ("Requesting pack #" ++ show p ++ " from "
                              ++ rNick ++ ", awaiting instructions…\n")
     o@(OfferFile _ f) <- requestFile p
     liftIO $ outputConcurrent ( "Received instructions for file "
                              ++ show (fileName f) ++ " of size "
                              ++ show (fileSize f) ++ " bytes.\n" )
     return o

-- TODO XDCC CANCEL on failure
requestFile :: Pack -> IrcIO OfferFile
requestFile num =
    do Context { remoteNick } <- ask
       sendAndWaitForAck ("XDCC SEND #" <> pack (show num))
                         (onFileOfferReceived remoteNick)
                         "Timeout when waiting for file offer."

onFileOfferReceived :: Nickname -> Broadcast OfferFile -> EventFunc
onFileOfferReceived rNick bc _ =
  onCtcpMessage (from rNick)
                (\ msg -> case runParser decodeService msg of
                            Right (FileTransfer o) -> broadcast bc o
                            _ -> return ())
