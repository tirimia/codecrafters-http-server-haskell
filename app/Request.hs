{-# LANGUAGE OverloadedStrings #-}

module Request (parseRequest) where

import qualified Data.ByteString.Char8 as BC

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
parseVerb other = Left $ ParseError $ "Unknown verb '" <> show other <> "'"

data HttpVersion = OneOne
  deriving (Show)

parseVersion :: BC.ByteString -> Either ParseError HttpVersion
parseVersion "HTTP/1.1" = Right OneOne
parseVersion other = Left $ ParseError $ "Unsupported HTTP version: " <> show other

data RequestLine = RequestLine
  { rVerb :: Verb,
    rTarget :: BC.ByteString,
    rVersion :: HttpVersion
  }
  deriving (Show)

splitRequestLine :: BC.ByteString -> Either ParseError (BC.ByteString, BC.ByteString, BC.ByteString)
splitRequestLine line = case BC.words line of
  [a, b, c] -> Right (a, b, c)
  xs -> Left $ ParseError $ "Expected 3 parts to the request line, got: " <> show (length xs)

parseRequestLine :: BC.ByteString -> Either ParseError RequestLine
parseRequestLine line = do
  (v, target, ver) <- splitRequestLine line
  verb <- parseVerb v
  version <- parseVersion ver
  pure $ RequestLine verb target version

type Headers = [(BC.ByteString, BC.ByteString)]

data Request = Request
  { rLine :: RequestLine,
    rHeaders :: Headers,
    rBody :: BC.ByteString
  }
  deriving (Show)

splitOn :: BC.ByteString -> BC.ByteString -> (BC.ByteString, BC.ByteString)
splitOn sep bytes = (a, BC.drop (BC.length sep) b)
  where
    (a, b) = BC.breakSubstring sep bytes

splitBy :: BC.ByteString -> BC.ByteString -> [BC.ByteString]
splitBy sep bytes = h : if BC.null t then [] else splitBy sep t
  where
    (h, t) = splitOn sep bytes

parseHeaders :: BC.ByteString -> Either ParseError Headers
parseHeaders bytes = mapM parseHeader headers
  where
    headers = splitBy crlf bytes
    parseHeader h = case splitOn ": " h of
      (_, v) | BC.null v -> Left $ ParseError $ "Header '" <> show h <> "' missing separator ': '"
      (k, v) -> Right (k, v)

parseRequest :: BC.ByteString -> Either ParseError Request
parseRequest bytes = do
  let (header, body) = splitOn doubleCrlf bytes
  let (requestLine, rawHeaders) = splitOn crlf header
  line <- parseRequestLine requestLine
  headers <- parseHeaders rawHeaders

  pure (Request line headers body)
