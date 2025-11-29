{-# LANGUAGE OverloadedStrings #-}

module Request (parseRequest) where

import qualified Data.ByteString.Char8 as BC

bail :: String -> Either ParseError b
bail = Left . ParseError

crlf :: BC.ByteString
crlf = "\r\n"

doubleCrlf :: BC.ByteString
doubleCrlf = crlf <> crlf

newtype ParseError = ParseError String
  deriving (Show)

data Verb = GET | POST
  deriving (Show)

parseVerb :: BC.ByteString -> Either ParseError Verb
parseVerb "GET" = Right GET
parseVerb "POST" = Right POST
parseVerb other = bail $ "Unknown verb '" <> BC.unpack other <> "'"

data HttpVersion = HTTP_1_1
  deriving (Show)

parseVersion :: BC.ByteString -> Either ParseError HttpVersion
parseVersion "HTTP/1.1" = Right HTTP_1_1
parseVersion other = bail $ "Unsupported HTTP version: " <> BC.unpack other

data RequestLine = RequestLine
  { rVerb :: Verb,
    rTarget :: BC.ByteString,
    rVersion :: HttpVersion
  }
  deriving (Show)

splitRequestLine :: BC.ByteString -> Either ParseError (BC.ByteString, BC.ByteString, BC.ByteString)
splitRequestLine line = case BC.words line of
  [a, b, c] -> Right (a, b, c)
  xs -> bail $ "Expected 3 parts to the request line, got: " <> show (length xs)

parseRequestLine :: BC.ByteString -> Either ParseError RequestLine
parseRequestLine line = do
  (verb, target, version) <- splitRequestLine line
  RequestLine <$> parseVerb verb <*> pure target <*> parseVersion version

type Headers = [(BC.ByteString, BC.ByteString)]

data Request = Request
  { rLine :: !RequestLine,
    rHeaders :: !Headers,
    rBody :: !BC.ByteString
  }
  deriving (Show)

splitOn :: BC.ByteString -> BC.ByteString -> (BC.ByteString, BC.ByteString)
splitOn sep bytes = (a, BC.drop (BC.length sep) b)
  where
    (a, b) = BC.breakSubstring sep bytes

splitBy :: BC.ByteString -> BC.ByteString -> [BC.ByteString]
splitBy sep bytes = a : if BC.null b then [] else splitBy sep b
  where
    (a, b) = splitOn sep bytes

parseHeaders :: BC.ByteString -> Either ParseError Headers
parseHeaders "" = Right mempty
parseHeaders bytes = mapM parseHeader headers
  where
    headers = splitBy crlf bytes
    parseHeader h = case splitOn ": " h of
      (_, v) | BC.null v -> bail $ "Header '" <> BC.unpack h <> "' missing separator ': '"
      (k, v) -> Right (k, v)

parseRequest :: BC.ByteString -> Either ParseError Request
parseRequest bytes =
  Request
    <$> parseRequestLine requestLine
    <*> parseHeaders rawHeaders
    <*> pure body
  where
    (header, body) = splitOn doubleCrlf bytes
    (requestLine, rawHeaders) = splitOn crlf header
