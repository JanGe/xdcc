{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module ZNC (autoConnectHooks) where

import           IRC.Types

import qualified Data.Text          as T (Text)
import           Network.IRC.Client (Event (..), EventHandler (..),
                                     EventType (EPrivmsg), Message (..),
                                     Source (..), StatefulIRC, send)

{- TODO Additional checks
   host == "znc.in"
   user == "znc"
-}
pattern ZncStatusUser = User "*status"

pattern ZncDisconnectedMsg target =
    Privmsg target (Right
        "You are currently disconnected from IRC. Use 'connect' to reconnect.")

autoConnectHooks :: Hook s
autoConnectHooks = Hook { onConnect    = return ()
                        , events       = [autoReconnect]
                        , onDisconnect = disconnect }

autoReconnect :: EventHandler s
autoReconnect = EventHandler
    { _description = "Auto-reconnect when ZNC is enabled"
    , _matchType   = EPrivmsg
    , _eventFunc   = handler
    }
  where
    handler Event { _source  = ZncStatusUser
                  , _message = ZncDisconnectedMsg _ } = send' "connect"
    handler _ = return ()

disconnect :: StatefulIRC s ()
disconnect = send' "disconnect"

send' :: T.Text -> StatefulIRC s ()
send' = send . Privmsg "*status" . Right
