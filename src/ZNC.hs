{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module ZNC (autoConnectHooks) where

import           IRC.Types

import           Control.Concurrent.STM (TVar, atomically, newTVar,
                                         readTVar, writeTVar)
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T (Text)
import           Network.IRC.Client     (Event (..), EventHandler (..),
                                         EventType (EPrivmsg), Message (..),
                                         Source (..), StatefulIRC, send)

{- TODO Additional checks
   host == "znc.in"
   user == "znc"
-}
pattern ZncStatusUser :: Source T.Text
pattern ZncStatusUser = User "*status"

pattern ZncDisconnectedMsg :: T.Text -> Message T.Text
pattern ZncDisconnectedMsg target =
    Privmsg target (Right
        "You are currently disconnected from IRC. Use 'connect' to reconnect.")

autoConnectHooks :: IO (Hook s)
autoConnectHooks = do
  wasConnected <- atomically $ newTVar True
  return Hook { onConnect    = return ()
              , events       = [autoReconnect wasConnected]
              , onDisconnect = disconnect wasConnected }

autoReconnect :: TVar Bool -> EventHandler s
autoReconnect wasConnected = EventHandler
    { _description = "Auto-reconnect when ZNC is enabled"
    , _matchType   = EPrivmsg
    , _eventFunc   = handler
    }
  where
    handler Event { _source  = ZncStatusUser
                  , _message = ZncDisconnectedMsg _ } = do
        liftIO $ atomically $ writeTVar wasConnected False
        send' "connect"
    handler _ = return ()

disconnect :: TVar Bool -> StatefulIRC s ()
disconnect wasConnected = do
    wasConnected' <- liftIO $ atomically $ readTVar wasConnected
    unless wasConnected' $ send' "disconnect"

send' :: T.Text -> StatefulIRC s ()
send' = send . Privmsg "*status" . Right
