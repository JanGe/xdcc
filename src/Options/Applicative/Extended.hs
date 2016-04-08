module Options.Applicative.Extended
  ( module Options.Applicative
  , tcpPort
  ) where

import           Control.Error              (readErr, throwE)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ask)
import           Network.Socket             (PortNumber)
import           Options.Applicative
import           Options.Applicative.Types  (ReadM (..))

tcpPort :: ReadM PortNumber
tcpPort = ReadM $ do
    s <- ask
    case readErr (errorMsg s) s :: Either String Int of
      Left e -> lift $ throwE (ErrorMsg e)
      Right p ->
        if p > 0 && p <= 65535
          then return (fromIntegral p)
          else lift $ throwE (ErrorMsg (errorMsg s))
  where errorMsg s = "cannot parse TCP port number " ++ s
