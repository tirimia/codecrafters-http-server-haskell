{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Request (getHeader, rHeaders, rLine, rTarget, runParseRequest)
import Response (echo, files, fourOhFour, index, serialize)
import System.Environment
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  let host = "127.0.0.1"
      port = "4221"
      dir = case args of
        ["--directory", d] -> Just d
        _ -> Nothing

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
    forkIO $ handleConn clientSocket dir

handleConn :: Socket -> Maybe String -> IO ()
handleConn clientSocket dir = do
  b <- recv clientSocket 4096
  resp <- case runParseRequest b of
    Left _ -> pure fourOhFour
    Right request -> case rTarget (rLine request) of
      "/" -> pure index
      "/user-agent" -> pure $ maybe fourOhFour echo (getHeader "user-agent" (rHeaders request))
      path | "/echo/" `BC.isPrefixOf` path -> pure . echo $ BC.drop 6 path
      path | "/files/" `BC.isPrefixOf` path ->
        case dir of
          Just d -> files (BC.unpack $ BC.drop 7 path) d
          Nothing -> pure fourOhFour
      _ -> pure fourOhFour
  _ <- send clientSocket $ serialize resp
  close clientSocket
