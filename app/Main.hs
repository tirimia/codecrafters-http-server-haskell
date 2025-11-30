{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Request (Verb (..), getHeader, rBody, rHeaders, rLine, rTarget, rVerb, runParseRequest, shouldKeepAlive, wantsGzip)
import Response (Response, closing, echo, fourOhFour, getFile, gzip, index, postFile, serialize)
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
handleConn clientSocket dir = handleRequest
  where
    handleRequest = do
      b <- recv clientSocket 4096
      if BC.null b
        then close clientSocket
        else do
          case runParseRequest b of
            Left _ -> do
              _ <- send clientSocket $ serialize fourOhFour
              close clientSocket
            Right request -> do
              resp <- catch (maybeClose request . maybeGzip request <$> route request) errorHandler
              _ <- send clientSocket $ serialize resp
              if shouldKeepAlive request then handleRequest else close clientSocket
    route request = case (rVerb $ rLine request, rTarget $ rLine request) of
      (GET, "/") -> pure index
      (GET, "/user-agent") -> pure $ maybe fourOhFour echo (getHeader "user-agent" (rHeaders request))
      (GET, path) | "/echo/" `BC.isPrefixOf` path -> pure . echo $ BC.drop 6 path
      (verb, path)
        | "/files/" `BC.isPrefixOf` path -> maybe (pure fourOhFour) operation dir
        where
          filePath = BC.unpack $ BC.drop 7 path
          operation = case verb of
            GET -> getFile filePath
            POST -> postFile (rBody request) filePath
      _ -> pure fourOhFour
    errorHandler :: SomeException -> IO Response
    errorHandler _ = pure fourOhFour
    maybeGzip req = if wantsGzip req then gzip else id
    maybeClose req = if shouldKeepAlive req then id else closing
