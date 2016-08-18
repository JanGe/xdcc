module IRC.Types
  ( Network
  , Channel
  , Nickname
  , Password
  , Hook(..)
) where

import           Data.ByteString.Char8 (ByteString)
import           Data.CaseInsensitive  (CI)
import qualified Data.Text             as T (Text)
import           Network.IRC.Client    (EventHandler, StatefulIRC)

type Network  = ByteString
type Channel  = CI T.Text
type Nickname = T.Text
type Password = T.Text

data Hook s = Hook { onConnect    :: StatefulIRC s ()
                   , events       :: [EventHandler s]
                   , onDisconnect :: StatefulIRC s () }
