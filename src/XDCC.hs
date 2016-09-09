{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module XDCC
    ( Env(..)
    , Pack(..)
    , initialState
    , putDccState
    , onAbort
    , dispatcher
    ) where

import qualified DCC
import           IRC.Types

import           Control.Concurrent.STM      (atomically, modifyTVar)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl(..), liftBaseOp)
import qualified Data.CaseInsensitive        as CI (mk)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T (Text, pack)
import qualified Network.IRC.Client          as IRC
import           System.Console.Concurrent   (outputConcurrent)
import           System.Signal               (Signal, installHandler, sigINT)

class XdccCommand a where
  toText :: a -> T.Text

data XdccSend
    = Send !Pack

instance XdccCommand XdccSend where
  toText (Send p) = "XDCC SEND #" <> packToText p

data XdccCancel
    = Cancel

instance XdccCommand XdccCancel where
  toText Cancel = "XDCC CANCEL"

newtype Pack = Pack { unpack :: Int }
    deriving (Eq, Show)

packToText :: Pack -> T.Text
packToText = T.pack . show . unpack

newtype XdccIO a = XdccIO { runXdccIO :: IRC.StatefulIRC Stati a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO XdccIO where
  type StM XdccIO a = a
  liftBaseWith f = XdccIO $ liftBaseWith $ \q -> f (q . runXdccIO)
  restoreM = XdccIO . restoreM

putState :: Status -> XdccIO ()
putState newS = XdccIO $ do
    state <- IRC.stateTVar
    liftIO . atomically . modifyTVar state $ \s -> s { xdccStatus = newS }

addHandler :: IRC.EventHandler Stati -> XdccIO ()
addHandler = XdccIO . IRC.addHandler

sendXdcc :: XdccCommand a => Nickname -> a -> XdccIO ()
sendXdcc nick cmd = XdccIO $ do
    liftIO $ outputConcurrent (show msg <> "\n")
    IRC.send msg
  where
    msg = IRC.Privmsg nick (Right $ toText cmd)

data Stati = Stati { xdccStatus :: Status
                   , dccStatus  :: DCC.Status
                   }

initialState :: Channel -> Stati
initialState chan = Stati { xdccStatus = WaitingForJoin chan
                          , dccStatus  = DCC.Requesting
                          }

data Env = Env { packNumber :: !Pack
               , dccEnv     :: !(DCC.Env Stati) }

data Status
    = WaitingForJoin !Channel
    | Joined
    deriving (Eq, Show)

dispatcher :: Env -> IRC.EventHandler Stati
dispatcher env = IRC.EventHandler
    { _description = "XDCC SEND workflow handling"
    , _matchType   = IRC.EEverything
    , _eventFunc   = \ev -> do
        status <- xdccStatus <$> IRC.state
        case status of
          WaitingForJoin chan -> runXdccIO $ joinedHandler env chan ev
          _                   -> return ()
    }

joinedHandler :: Env -> Channel -> IRC.UnicodeEvent -> XdccIO ()
joinedHandler Env {..} channel IRC.Event { _message = IRC.Join joined }
    | CI.mk joined == channel = do
        putState Joined
        liftIO $ outputConcurrent ( "Joined " <> joined <> ".\n")

        liftIO $ outputConcurrent
            ( "Requesting pack #" <> packToText packNumber <> " from " <> rNick
           <> ", awaiting file offer…\n" )
        addHandler (dispatcherDcc dccEnv)
        onInterruptSignal $ const $ sendXdcc rNick Cancel
        sendXdcc rNick (Send packNumber)
  where
    rNick = DCC.remoteNick dccEnv
joinedHandler _ _ _ = return ()

onAbort :: Nickname -> IRC.StatefulIRC Stati ()
onAbort rNick = runXdccIO $ sendXdcc rNick Cancel

onInterruptSignal :: (Signal -> XdccIO ()) -> XdccIO ()
onInterruptSignal f = installHandler' sigINT $ \s -> do
    f s
    XdccIO IRC.disconnect
  where
    installHandler' = liftBaseOp . installHandler

putDccState :: DCC.Status -> IRC.StatefulIRC Stati ()
putDccState newS = do
    state <- IRC.stateTVar
    liftIO . atomically . modifyTVar state $ \s -> s { dccStatus = newS }

dispatcherDcc :: DCC.Env Stati -> IRC.EventHandler Stati
dispatcherDcc env = IRC.EventHandler
    { _description = "DCC SEND workflow handling"
    , _matchType   = IRC.EEverything
    , _eventFunc   = \ev -> do
        status <- dccStatus <$> IRC.state
        case status of
          DCC.Requesting        -> DCC.runDccIO env $ DCC.offerReceivedHandler ev
          DCC.TryResuming offer -> DCC.runDccIO env $ DCC.acceptResumeHandler offer ev
          _                     -> return ()
    }
