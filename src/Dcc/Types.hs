{-# LANGUAGE NamedFieldPuns #-}

module Dcc.Types ( File
                 , FileMetadata (..)
                 , Token
                 , Protocol (..)
                 , fileMetadata
                 , ipFromNetworkByteOrder
                 , ipToNetworkByteOrder
                 ) where

import           Data.Binary           (byteSwap32)
import           Data.ByteString.Char8 (ByteString)
import           Data.IP               (IPv4, fromHostAddress, toHostAddress)
import           Network.Socket        (PortNumber)
import           System.IO.Streams     (OutputStream)

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

ipFromNetworkByteOrder :: Integer -> IPv4
ipFromNetworkByteOrder = fromHostAddress . byteSwap32 . fromIntegral

ipToNetworkByteOrder :: IPv4 -> Integer
ipToNetworkByteOrder = fromIntegral . byteSwap32 . toHostAddress
