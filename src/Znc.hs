{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Znc (autoConnectHooks) where

import           IRC.Types

import           Control.Monad     (when)
import           Data.Monoid       ((<>))
import           Network.SimpleIRC (Command (MPrivmsg), IrcEvent (Privmsg),
                                    IrcMessage (..), sendCmd)

autoConnectHooks :: Hook
autoConnectHooks = Hook { onConnect    = const (return ())
                        , events       = [autoReconnect]
                        , onDisconnect = disconnect }

autoReconnect :: IrcEvent
autoReconnect = Privmsg (\con msg ->
    when (isDisconnectedMsg msg) $
        sendCmd con $ MPrivmsg "*status" "connect" )

disconnect :: Connection -> IO ()
disconnect con = sendCmd con $ MPrivmsg "*status" "disconnect"

isDisconnectedMsg :: IrcMessage -> Bool
isDisconnectedMsg msg@IrcMessage { mMsg } =
    isStatusMsg msg
 && mMsg == ( "You are currently disconnected from IRC. Use 'connect' to "
           <> "reconnect." )

isStatusMsg :: IrcMessage -> Bool
isStatusMsg IrcMessage { mHost, mOrigin, mNick, mUser } =
    mHost == Just "znc.in"  && mOrigin == Just "*status"
 && mNick == Just "*status" && mUser   == Just "znc"
