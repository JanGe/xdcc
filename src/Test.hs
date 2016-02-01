import           Dcc.Parser
import           Dcc.Types

import           Data.Binary           (byteSwap32)
import           Data.ByteString.Char8 (pack)
import           Data.IP               (fromHostAddress)
import           Network.IRC.CTCP      (encodeCTCP)
import           Text.Parse.ByteString (runParser)

-- import           Test.QuickCheck

-- DCC SEND 2342q3.0234lsdfsaef-sadfseaf.txt 1489410986 52368 1012239271
parseDccOffer :: String -> Int -> Int -> Integer -> Bool
parseDccOffer fileName ip port fileSize =
  parseSendAction (encodeCTCP (pack ( "DCC SEND "
                                    ++ fileName ++ " "
                                    ++ show ip ++ " "
                                    ++ show port ++ " "
                                    ++ show fileSize ))
  ) == Right (Dcc FileMetadata { fileName = fileName
                               , fileSize = fileSize}
                  (fromHostAddress . byteSwap32 . fromIntegral $ ip)
                  (fromIntegral port))

main = print $ parseDccOffer
         "2342q3.0234lsdfsaef-sadfseaf.txt"
         1489410986 52368 1012239271
