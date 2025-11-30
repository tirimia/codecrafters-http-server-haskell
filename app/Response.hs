{-# LANGUAGE OverloadedStrings #-}

module Response (Response, echo, getFile, gzip, postFile, fourOhFour, index, serialize) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BCL
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
    (Headers [("content-type", "text/plain"), ("content-length", BC.pack $ show $ BC.length s)])
    s

gzip :: Response -> Response
gzip (Response line (Headers hs) body) =
  Response line (Headers $ newHeaders ++ filteredHeaders) compressedBody
  where
    compressedBody = BCL.toStrict $ GZip.compress (BCL.fromStrict body)
    newHeaders =
      [ ("content-encoding", "gzip"),
        ("content-length", BC.pack . show $ BC.length compressedBody)
      ]
    filteredHeaders = filter ((`notElem` ["content-encoding", "content-length"]) . fst) hs

-- TODO: look into lenses to use echo and modify the content-type
getFile :: String -> String -> IO Response
getFile file dir = replyWith $ dir <> file
  where
    replyWith path = do
      content <- BC.readFile path
      pure $
        Response
          (ResponseLine HTTP_1_1 OK)
          (Headers [("content-type", "application/octet-stream"), ("content-length", BC.pack $ show $ BC.length content)])
          content

postFile :: BC.ByteString -> String -> String -> IO Response
postFile content file dir = do
  BC.writeFile (dir <> file) content
  pure $
    Response
      (ResponseLine HTTP_1_1 Created)
      (Headers mempty)
      mempty

class Serialize a where
  serialize :: a -> BC.ByteString

data Code = OK | Created | NotFound
  deriving (Show)

instance Serialize HttpVersion where
  serialize v = case v of
    HTTP_1_1 -> "HTTP/1.1"

instance Serialize Code where
  serialize code = case code of
    OK -> "200 OK"
    Created -> "201 Created"
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
