module Xdcc ( module Dcc
            , request
            ) where

import           Dcc

import           Control.Concurrent.Broadcast (Broadcast, broadcast)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask)
import           Data.ByteString.Char8        (pack)
import           System.Console.Concurrent    (outputConcurrent)

request :: Pack -> DccIO OfferFile
request p =
  do env <- ask
     liftIO $ outputConcurrent ( "Requesting pack #" ++ show p ++ " from "
                              ++ remoteNick env ++ ", awaiting file offer…\n" )
     o@(OfferFile _ f) <- requestFile p
     liftIO $ outputConcurrent
                ( "Received file offer for " ++ show (fileName f)
               ++ (case fileSize f of
                     Just fs -> " of size " ++ show fs ++ " bytes.\n"
                     Nothing -> ", no file size provided.\n") )
     return o

-- TODO XDCC CANCEL on failure
requestFile :: Pack -> DccIO OfferFile
requestFile num =
    do env <- ask
       lift $ sendAndWaitForAck (connection env)
                                (remoteNick env)
                                (pack ("XDCC SEND #" ++ show num))
                                onFileOfferReceived
                                "Timeout when waiting for file offer."

onFileOfferReceived :: Nickname -> Broadcast OfferFile -> EventFunc
onFileOfferReceived rNick bc _ =
  onCtcpMessage (from rNick)
                (\ msg -> case runParser parseService msg of
                            Right (FileTransfer o) -> broadcast bc o
                            _ -> return ())
