module IRC.Types
  ( IrcIO
  , IrcParams(..)
  , Network
  , Channel
  , Nickname
  , Password
  , Pack
  , Connection
  , Hook(..)
  , Options(..)
) where

import           Control.Error        (ExceptT)
import           Data.CaseInsensitive (CI)
import           Network.SimpleIRC    (IrcEvent, MIrc)
import           Network.Socket       (PortNumber)
import           Data.IP              (IPv4)


type Network = String
type Channel = CI String
type Nickname = String
type Password = String
type Pack = Int
type Connection = MIrc

type IrcIO = ExceptT String IO

data IrcParams = IrcParams { host     :: Network
                           , port     :: PortNumber
                           , secure   :: Bool
                           , username :: Nickname
                           , password :: Maybe Password
                           , nickname :: Nickname
                           , channels :: [Channel]
                           , hooks    :: [Hook] }

data Hook = Hook { onConnect    :: Connection -> IO ()
                 , onEvent      :: [IrcEvent]
                 , onDisconnect :: Connection -> IO () }

data Options = Options { network            :: Network
                       , mainChannel        :: Channel
                       , rNick              :: Nickname
                       , packno             :: Pack
                       , rPort              :: PortNumber
                       , usesecure          :: Bool
                       , user               :: Nickname
                       , pass               :: Maybe Password
                       , nick               :: Nickname
                       , additionalChannels :: [Channel]
                       , publicIp           :: Maybe IPv4
                       , lPort              :: Maybe PortNumber
                       , zncAutoConnect     :: Bool
                       , verbose            :: Bool }
    deriving (Show)
