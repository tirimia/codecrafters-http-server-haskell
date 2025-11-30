{-# LANGUAGE OverloadedStrings #-}

module Response (echo, files, fourOhFour, index, serialize) where

import Control.Exception
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

-- TODO: look into lenses to use echo and modify the content-type
files :: String -> String -> IO Response
files file dir = catch (replyWith $ dir <> file) handler
  where
    handler :: SomeException -> IO Response
    handler e = pure $ fourOhFour
    replyWith path = do
      content <- BC.readFile path
      pure $
        Response
          (ResponseLine HTTP_1_1 OK)
          (Headers [("Content-Type", "application/octet-stream"), ("Content-Length", BC.pack $ show $ BC.length content)])
          content

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
