{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Request (getHeader, rHeaders, rLine, rTarget, runParseRequest)
import Response (echo, fourOhFour, index, serialize)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
    b <- recv clientSocket 4096
    let req = runParseRequest b
    let resp = case req of
          Left _ -> fourOhFour
          Right request -> case rTarget (rLine request) of
            "/" -> index
            "/user-agent" -> maybe fourOhFour echo (getHeader "user-agent" (rHeaders request))
            path | "/echo/" `BC.isPrefixOf` path -> echo $ BC.drop 6 path
            _ -> fourOhFour
    _ <- send clientSocket $ serialize resp

    close clientSocket
