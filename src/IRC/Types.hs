module IRC.Types
  ( Network
  , Channel
  , Nickname
  , Password
  , Hook(..)
  , addHandler'
) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (readTVar, writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString.Char8       (ByteString)
import           Data.CaseInsensitive        (CI)
import qualified Data.Text                   as T (Text)
import           Network.IRC.Client          (EventHandler, InstanceConfig (..),
                                              StatefulIRC, instanceConfigTVar)

type Network  = ByteString
type Channel  = CI T.Text
type Nickname = T.Text
type Password = T.Text

data Hook s = Hook { onConnect    :: StatefulIRC s ()
                   , events       :: [EventHandler s]
                   , onDisconnect :: StatefulIRC s ()
                   }

-- TODO Remove when https://github.com/barrucadu/irc-client/pull/19 is merged.
-- | Add an event handler
addHandler' :: EventHandler s -> StatefulIRC s ()
addHandler' handler = do
    tvarI <- instanceConfigTVar

    liftIO . atomically $  do
        iconf <- readTVar tvarI
        writeTVar tvarI iconf { _eventHandlers = handler : _eventHandlers iconf }
