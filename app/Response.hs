{-# LANGUAGE OverloadedStrings #-}

module Response (echo, fourOhFour, index, serialize) where

import qualified Data.ByteString.Char8 as BC
import Request (Headers (..), HttpVersion (..))

index :: Response
index =
  Response
    (ResponseLine HTTP_1_1 OK)
    (Headers [])
    mempty

fourOhFour :: Response
fourOhFour =
  Response
    (ResponseLine HTTP_1_1 NotFound)
    (Headers [])
    mempty

echo :: BC.ByteString -> Response
echo s =
  Response
    (ResponseLine HTTP_1_1 OK)
    (Headers [("Content-Type", "text/plain"), ("Content-Length", BC.pack $ show $ BC.length s)])
    s

class Serialize a where
  serialize :: a -> BC.ByteString

data Code = NotFound | OK
  deriving (Show)

instance Serialize HttpVersion where
  serialize v = case v of
    HTTP_1_1 -> "HTTP/1.1"

instance Serialize Code where
  serialize code = case code of
    OK -> "200 OK"
    NotFound -> "404 Not Found"

instance Serialize Headers where
  serialize (Headers hs) = BC.concat . map serializeHeader $ hs
    where
      serializeHeader h = fst h <> ": " <> snd h <> "\r\n"

data ResponseLine = ResponseLine
  { rVersion :: !HttpVersion,
    rCode :: !Code
  }
  deriving (Show)

instance Serialize ResponseLine where
  serialize (ResponseLine version code) = BC.intercalate " " [serialize version, serialize code]

data Response = Response
  { rLine :: !ResponseLine,
    rHeaders :: !Headers,
    rBody :: !BC.ByteString
  }
  deriving (Show)

instance Serialize Response where
  serialize (Response line headers body) = BC.intercalate "\r\n" [serialize line, serialize headers, body]
