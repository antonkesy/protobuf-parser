module ProtoParser.Comment
  ( parseComment,
    parseComment',
    parseSingleLineComment,
    parseMultiLineComment,
    removeComment,
  )
where

import Control.Monad (void)
import ProtoParser.Misc (eol)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseComment' :: Protobuf -> Parser Protobuf
-- TODO: write try before do?
parseComment' p = do
  _ <- parseComment
  return p

removeComment :: Parser ()
removeComment = do
  -- TODO: correct way to try?
  void (try parseSingleLineComment <|> try parseMultiLineComment)

parseComment :: Parser Comment
parseComment = do
  -- TODO: correct way to try?
  try parseSingleLineComment <|> try parseMultiLineComment

parseSingleLineComment :: Parser Comment
parseSingleLineComment = do
  _ <- string "//"
  manyTill anyChar (try eol)

parseMultiLineComment :: Parser Comment
parseMultiLineComment = do
  _ <- string "/*"
  manyTill anyChar (try (string "*/"))
