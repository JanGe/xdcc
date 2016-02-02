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
    do _ <- literal "DCC SEND"
       space
       file <- parseFileName
       space
       ip <- parseIpBE
       space
       onFail
         (do port <- parseTcpPort
             space
             size <- parseUnsignedInteger
             return (Dcc (FileMetadata file size) ip port))
         (do _ <- literal "0"
             space
             size <- parseUnsignedInteger
             space
             token <- parseToken
             return (ReverseDcc (FileMetadata file size)
                                ip
                                token))

acceptActionParser :: Protocol -> Parser Integer
acceptActionParser (Dcc f _ p) =
    do skipAcceptActionPrefix f
       _ <- literal (show p)
       space
       parseUnsignedInteger
acceptActionParser (ReverseDcc f _ t) =
    do skipAcceptActionPrefix f
       _ <- literal "0"
       space
       pos <- parseUnsignedInteger
       space
       _ <- literal t
       return pos

skipAcceptActionPrefix :: FileMetadata -> Parser ()
skipAcceptActionPrefix f =
    do _ <- literal "DCC ACCEPT"
       space
       _ <- literal (fileName f)
       space

space :: Parser ()
space = do _ <- literal " "
           return ()

parseFileName :: Parser FilePath
parseFileName = (takeFileName . Lazy.unpack) <$> many1Satisfy (/= ' ')

parseIpBE :: Parser IPv4
parseIpBE = ipFromNetworkByteOrder <$> parseBoundedInteger 1 4294967295

parseTcpPort :: Parser PortNumber
parseTcpPort = fromInteger <$> parseBoundedInteger 1 65535

parseBoundedInteger :: Integer -> Integer -> Parser Integer
parseBoundedInteger lower upper = do
    num <- parseUnsignedInteger
    when (num < lower || num > upper) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show lower ++ ", " ++ show upper ++ "].")
    return num

parseToken :: Parser Token
parseToken = Lazy.unpack <$> many1Satisfy (/= ' ')
