module ProtoParser.Comment
  ( parseComment,
    parseComment',
    parseSingleLineComment,
    parseMultiLineComment,
  )
where

import Control.Monad (void)
import ProtoParser.Misc (eol)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseComment' :: Parser Protobuf
parseComment' = do
  _ <- parseComment
  return (Protobuf {package = [], imports = [], options = [], enums = [], messages = [], services = []})

removeComment :: Parser ()
removeComment = do
  -- TODO: correct way to try?
  void (try parseSingleLineComment <|> try parseMultiLineComment)


parseComment :: Parser Comment
parseComment = do
  -- TODO: correct way to try?
  try parseSingleLineComment <|> try parseMultiLineComment

-- TODO: these comments could be anywhere
parseSingleLineComment :: Parser Comment
parseSingleLineComment = do
  between (string "//") eol (many anyChar)

-- TODO: these comments could be anywhere (same as spaces)
parseMultiLineComment :: Parser Comment
parseMultiLineComment = try $ do
  _ <- string "/*"
  manyTill anyChar (try (string "*/"))
