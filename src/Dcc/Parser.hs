module Dcc.Parser ( parseSendAction
                  , parseAcceptAction
                  ) where

import           Dcc.Types

import           Control.Monad              (when)
import qualified Data.ByteString.Lazy.Char8 as Lazy (fromStrict, unpack)
import           Data.IP                    (IPv4)
import           Network.IRC.CTCP           (CTCPByteString, decodeCTCP)
import           Network.Socket             (PortNumber)
import           System.FilePath            (takeFileName)
import           Text.Parse.ByteString      (Parser, literal, many1Satisfy,
                                             onFail, parseUnsignedInteger,
                                             runParser)

parseSendAction :: CTCPByteString -> Either String Protocol
parseSendAction = parseAction sendActionParser

parseAcceptAction :: Protocol -> CTCPByteString -> Either String Integer
parseAcceptAction = parseAction . acceptActionParser

parseAction :: Parser a -> CTCPByteString -> Either String a
parseAction parser = fst . runParser parser . Lazy.fromStrict . decodeCTCP

-- TODO Use another parser framework
sendActionParser :: Parser Protocol
sendActionParser =
    do literal "DCC SEND"
       skipSpace
       fileName <- parseFileName
       skipSpace
       ip <- parseIpBE
       skipSpace
       onFail
         (do port <- parseTcpPort
             skipSpace
             fileSize <- parseUnsignedInteger
             return (Dcc (FileMetadata fileName fileSize) ip port))
         (do literal "0"
             skipSpace
             fileSize <- parseUnsignedInteger
             skipSpace
             token <- parseToken
             return (ReverseDcc (FileMetadata fileName fileSize)
                                ip
                                token))

acceptActionParser :: Protocol -> Parser Integer
acceptActionParser (Dcc f _ p) =
    do skipAcceptActionPrefix f
       literal (show p)
       skipSpace
       parseUnsignedInteger
acceptActionParser (ReverseDcc f _ t) =
    do skipAcceptActionPrefix f
       literal "0"
       skipSpace
       pos <- parseUnsignedInteger
       skipSpace
       literal t
       return pos

skipAcceptActionPrefix :: FileMetadata -> Parser ()
skipAcceptActionPrefix f =
    do literal "DCC ACCEPT"
       skipSpace
       literal (fileName f)
       skipSpace

skipSpace :: Parser ()
skipSpace = do literal " "
               return ()

parseFileName :: Parser FilePath
parseFileName = (takeFileName . Lazy.unpack) <$> many1Satisfy (/= ' ')

parseIpBE :: Parser IPv4
parseIpBE = ipFromNetworkByteOrder <$> parseBoundedInteger 1 4294967295

parseTcpPort :: Parser PortNumber
parseTcpPort = fromInteger <$> parseBoundedInteger 1 65535

parseBoundedInteger :: Integer -> Integer -> Parser Integer
parseBoundedInteger min max = do
    num <- parseUnsignedInteger
    when (num < min || num > max) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show min ++ ", " ++ show max ++ "].")
    return num

parseToken :: Parser Token
parseToken = Lazy.unpack <$> many1Satisfy (/= ' ')
