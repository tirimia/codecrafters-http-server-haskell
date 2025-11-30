{-# LANGUAGE OverloadedStrings #-}

module Parser (Parser (..), Error (..), crlf, space, string, untilCRLF, untilSpace, takeRest, takeWhile, sepBy) where

import Control.Applicative
import qualified Data.ByteString as BC
import Data.List (nub)
import Data.Word (Word8)
import Prelude hiding (takeWhile)

newtype Error
  = Error String
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: BC.ByteString -> Either [Error] (a, BC.ByteString)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Left [Error "Empty input"]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err -> case r input of
        Left err' -> Left $ nub $ err <> err'
        right -> right
      right -> right

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy predicate = Parser $ \input ->
  case BC.uncons input of
    Nothing -> Left [Error "End of input reached prematurely"]
    Just (hd, rest)
      | predicate hd -> Right (hd, rest)
      | otherwise -> Left [Error $ "Unexpected byte" <> show hd]

char :: Word8 -> Parser Word8
char i = satisfy (== i)

untilString :: BC.ByteString -> Parser BC.ByteString
untilString needle = Parser $ \input ->
  case BC.breakSubstring needle input of
    (_, rest) | BC.null rest -> Left [Error $ "Looked for '" <> show needle <> "' but could not find it"]
    (chunk, rest) -> Right (chunk, BC.drop (BC.length needle) rest)

takeWhile :: (Word8 -> Bool) -> Parser BC.ByteString
takeWhile predicate = Parser $ \input ->
  Right (BC.takeWhile predicate input, BC.dropWhile predicate input)

string :: BC.ByteString -> Parser BC.ByteString
string target = Parser $ \input ->
  if BC.isPrefixOf target input
    then Right (target, BC.drop (BC.length target) input)
    else Left [Error $ "Could not find " <> show target]

crlf :: Parser BC.ByteString
crlf = string "\r\n"

space :: Parser Word8
space = char 32

untilCRLF :: Parser BC.ByteString
untilCRLF = untilString "\r\n"

untilSpace :: Parser BC.ByteString
untilSpace = untilString " "

takeRest :: Parser BC.ByteString
takeRest = Parser $ \input -> Right (input, BC.empty)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p `sepBy'` sep) <|> pure []
  where
    sepBy' p' sep' = (:) <$> p' <*> many (sep' *> p')
