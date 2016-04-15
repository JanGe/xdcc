module Options.Applicative.Extended
  ( module Options.Applicative
  , tcpPort
  ) where

import           Data.Word           (Word16)
import           Network.Socket      (PortNumber)
import           Options.Applicative
import           Text.Read           (readMaybe)

tcpPort :: ReadM PortNumber
tcpPort =
    eitherReader (\ arg ->
        case readMaybe arg :: Maybe Word16 of
          Just n | n > 0 -> Right (fromIntegral n)
          _              -> Left ("Cannot parse TCP port number: " ++ arg) )
