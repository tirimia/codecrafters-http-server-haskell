{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
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
  let addr = case addrInfo of
        [] -> error "No address info found"
        (a : _) -> a
  serverSocket <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ addrAddress addr
  listen serverSocket 5

  forever $ do
    (clientSocket, _) <- accept serverSocket
    forkIO $ handleConn clientSocket

handleConn :: Socket -> IO ()
handleConn clientSocket = do
  b <- recv clientSocket 4096
  let resp = case runParseRequest b of
        Left _ -> fourOhFour
        Right request -> case rTarget (rLine request) of
          "/" -> index
          "/user-agent" -> maybe fourOhFour echo (getHeader "user-agent" (rHeaders request))
          path | "/echo/" `BC.isPrefixOf` path -> echo $ BC.drop 6 path
          _ -> fourOhFour
  _ <- send clientSocket $ serialize resp
  close clientSocket
