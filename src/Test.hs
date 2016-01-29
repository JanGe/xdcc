import           Dcc.Parser
import           Dcc.Types

import           Data.ByteString.Lazy.Char8 (pack)
import           Data.IP                    (toIPv4)
import           Text.Parse.ByteString      (runParser)

-- import           Test.QuickCheck

-- DCC SEND 2342q3.0234lsdfsaef-sadfseaf.txt 1489410986 52368 1012239271
parseDccOffer :: String -> Int -> Int -> Integer -> Bool
parseDccOffer fileName ip port fileSize =
  fst (runParser offerParserDcc (pack ( "DCC SEND " ++
                                        fileName ++ " " ++
                                        show ip ++ " " ++
                                        show port ++ " " ++
                                        show fileSize )
  )) == Right (Dcc FileMetadata { fileName = "2342q3.0234lsdfsaef-sadfseaf.txt"
                                , fileSize = 1012239271}
                   (toIPv4 [88, 198, 155, 170])
                   52368)

main = print $ parseDccOffer
         "2342q3.0234lsdfsaef-sadfseaf.txt"
         1489410986 52368 1012239271
