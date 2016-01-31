module Dcc.Parser ( parseDccProtocol
                  , parseAcceptPosition
                  ) where

import           Dcc.Types

import           Control.Error              (hush)
import           Control.Monad              (when)
import           Data.Binary                (byteSwap32)
import qualified Data.ByteString.Lazy.Char8 as Lazy (ByteString, fromStrict, toStrict,
                                                     unpack)
import           Data.IP                    (IPv4, fromHostAddress)
import           Network.IRC.CTCP           (CTCPByteString, decodeCTCP)
import           Network.Socket             (PortNumber)
import           System.FilePath            (takeFileName)
import           Text.Parse.ByteString      (Parser, literal, many1Satisfy,
                                             onFail, parseUnsignedInteger,
                                             runParser)

parseDccProtocol :: CTCPByteString -> Either String Protocol
parseDccProtocol = parseMsg offerParser

parseAcceptPosition :: Protocol -> CTCPByteString -> Either String Integer
parseAcceptPosition = parseMsg . acceptParser

parseMsg :: Parser a -> CTCPByteString -> Either String a
parseMsg parser = fst . runParser parser . Lazy.fromStrict . decodeCTCP

-- TODO Use another parser framework
offerParser :: Parser Protocol
offerParser = dccParser `onFail` reverseDccParser
  where commonPrefix = do literal "DCC SEND"
                          skipSpace
                          fileName <- parseFileName
                          skipSpace
                          ip <- parseIpBE
                          skipSpace
                          return (fileName, ip)
        dccParser = do (fileName, ip) <- commonPrefix
                       port <- parseTcpPort
                       skipSpace
                       fileSize <- parseUnsignedInteger
                       return (Dcc (FileMetadata fileName fileSize) ip port)
        reverseDccParser = do (fileName, ip) <- commonPrefix
                              literal (show 0)
                              skipSpace
                              fileSize <- parseUnsignedInteger
                              skipSpace
                              token <- parseToken
                              return (ReverseDcc (FileMetadata fileName fileSize)
                                                 ip
                                                 token)

acceptParser :: Protocol -> Parser Integer
acceptParser (Dcc f p _) = do literal "DCC ACCEPT"
                              skipSpace
                              literal (fileName f)
                              skipSpace
                              literal (show p)
                              skipSpace
                              parseUnsignedInteger
acceptParser (ReverseDcc f _ t) = do literal "DCC ACCEPT"
                                     skipSpace
                                     literal (fileName f)
                                     skipSpace
                                     literal (show 0)
                                     skipSpace
                                     pos <- parseUnsignedInteger
                                     skipSpace
                                     literal t
                                     return pos

parseBoundedInteger :: Integer -> Integer -> Parser Integer
parseBoundedInteger min max = do
    num <- parseUnsignedInteger
    when (num < min || num > max) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show min ++ ", " ++ show max ++ "].")
    return num

parseTcpPort :: Parser PortNumber
parseTcpPort = fromInteger <$> parseBoundedInteger 1 65535

parseFileName :: Parser FilePath
parseFileName = (takeFileName . Lazy.unpack) <$> many1Satisfy (/= ' ')

parseIpBE :: Parser IPv4
parseIpBE = fromHostAddress . byteSwap32 . fromIntegral <$>
              parseBoundedInteger 1 4294967295

skipSpace :: Parser String
skipSpace = literal " "

parseToken :: Parser Token
parseToken = Lazy.unpack <$> many1Satisfy (/= ' ')
