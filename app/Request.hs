{-# LANGUAGE OverloadedStrings #-}

module Request (runParseRequest, Request (..), RequestLine (..), Verb (..), HttpVersion (..), Headers (..), getHeader) where

import Control.Applicative (many, (<|>))
import qualified Data.ByteString as BC
import qualified Data.ByteString.Char8 as BC8
import Data.Char (ord, toLower)
import Data.Functor (($>))
import Data.Word (Word8)
import Parser (Error (..), Parser (..), crlf, space, string, takeRest, takeWhile, untilCRLF, untilSpace)
import Prelude hiding (takeWhile)

data Verb = GET | POST deriving (Show, Eq)

data HttpVersion = HTTP_1_1 deriving (Show, Eq)

data RequestLine = RequestLine
  { rVerb :: Verb,
    rTarget :: BC.ByteString,
    rVersion :: HttpVersion
  }
  deriving (Show)

newtype Headers = Headers [(BC.ByteString, BC.ByteString)]
  deriving (Show)

getHeader :: BC.ByteString -> Headers -> Maybe BC.ByteString
getHeader header (Headers hs) = lookup header hs

data Request = Request
  { rLine :: !RequestLine,
    rHeaders :: !Headers,
    rBody :: !BC.ByteString
  }
  deriving (Show)

failWith :: String -> Parser a
failWith e = Parser $ \_ -> Left [Error e]

verbParser :: Parser Verb
verbParser =
  (string "GET" $> GET)
    <|> (string "POST" $> POST)
    <|> failWith "Unsupported verb"

versionParser :: Parser HttpVersion
versionParser =
  (string "HTTP/1.1" $> HTTP_1_1)
    <|> failWith "Unsupported HTTP version"

requestLineParser :: Parser RequestLine
requestLineParser =
  RequestLine <$> verbParser <*> (space *> untilSpace) <*> (versionParser <* crlf)

colon :: Word8
colon = fromIntegral . ord $ ':'

headerParser :: Parser (BC.ByteString, BC.ByteString)
headerParser = do
  key <- BC8.map toLower <$> takeWhile (/= colon)
  _ <- string ": "
  value <- untilCRLF
  pure (key, value)

headersParser :: Parser Headers
headersParser = Headers <$> many headerParser

requestParser :: Parser Request
requestParser =
  Request <$> requestLineParser <*> headersParser <*> (crlf *> takeRest)

runParseRequest :: BC.ByteString -> Either String Request
runParseRequest bytes = case runParser requestParser bytes of
  Left errs -> Left $ show errs
  Right (req, _) -> Right req
