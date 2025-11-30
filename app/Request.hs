{-# LANGUAGE OverloadedStrings #-}

module Request (runParseRequest, Request (..), RequestLine (..), Verb (..), HttpVersion (..), Headers (..), getHeader, wantsGzip) where

import Control.Applicative (many, (<|>))
import qualified Data.ByteString as BC
import qualified Data.ByteString.Char8 as BC8
import Data.Char (ord, toLower)
import Data.Functor (($>))
import Data.Word (Word8)
import Parser (Error (..), Parser (..), crlf, sepBy, space, string, takeRest, takeWhile, untilCRLF, untilSpace)
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

w8char :: Char -> Word8
w8char = fromIntegral . ord

headerParser :: Parser (BC.ByteString, BC.ByteString)
headerParser = do
  key <- BC8.map toLower <$> takeWhile (/= w8char ':')
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

headerValueListParser :: Parser [BC.ByteString]
headerValueListParser = sepBy value (string ", ")
  where
    value = takeWhile (\b -> b /= w8char ',' && b /= w8char ' ')

wantsGzip :: Request -> Bool
wantsGzip (Request _ hs _) =
  maybe False (elem "gzip") (toMaybe . runParser headerValueListParser =<< getHeader "accept-encoding" hs)
  where
    toMaybe = either (const Nothing) (Just . fst)
