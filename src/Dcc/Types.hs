module Dcc.Types ( Context (..)
                 , File
                 , FileMetadata (..)
                 , Token
                 , Protocol (..)
                 , fileMetadata
                 ) where

import           Irc

import           Data.ByteString.Char8 (ByteString)
import           Data.IP               (IPv4)
import           Network.Socket        (PortNumber)
import           System.IO.Streams     (OutputStream)

data Context = Context { remoteNick :: Nickname
                       , publicIp   :: Maybe IPv4 }
               deriving (Eq, Show)

type File = OutputStream ByteString

data FileMetadata = FileMetadata { fileName :: FilePath
                                 , fileSize :: Integer }
                    deriving (Eq, Show)

type Token = String

data Protocol = Dcc FileMetadata IPv4 PortNumber
              | ReverseDcc FileMetadata IPv4 Token
              deriving (Eq, Show)

fileMetadata :: Protocol -> FileMetadata
fileMetadata (Dcc f _ _) = f
fileMetadata (ReverseDcc f _ _) = f
