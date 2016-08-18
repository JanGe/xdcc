module Options.Applicative.Extended
  ( module Options.Applicative
  , tcpPort
  , packNum
  ) where

import           Data.Word           (Word16)
import           Network.Socket      (PortNumber)
import           Options.Applicative
import           Text.Read           (readMaybe)
import           XDCC                (Pack (..))

tcpPort :: ReadM PortNumber
tcpPort =
    eitherReader (\ arg ->
        case readMaybe arg :: Maybe Word16 of
          Just n | n > 0 -> Right (fromIntegral n)
          _              -> Left ("Cannot parse TCP port number: " ++ arg) )

packNum :: ReadM Pack
packNum =
    eitherReader (\ arg ->
        case readMaybe arg :: Maybe Int of
          Just n | n > 0 -> Right (Pack n)
          _              -> Left ("Cannot parse XDCC pack number: " ++ arg) )
