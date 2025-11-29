{-# LANGUAGE OverloadedStrings #-}

module Response (echo) where

import qualified Data.ByteString.Char8 as BC

echo :: BC.ByteString -> BC.ByteString
echo s = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> (BC.pack . show . BC.length) s <> "\r\n\r\n" <> s
