module Text.Protobuf.Parser.Comment
  ( parseComment,
    parseComment',
    parseSingleLineComment,
    parseMultiLineComment,
    removeComment,
  )
where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.EndOfLine (eol)
import Text.Protobuf.Types

parseComment' :: Protobuf -> Parser Protobuf
parseComment' p = do
  _ <- parseComment
  return p

removeComment :: Parser ()
removeComment = void (try parseSingleLineComment <|> try parseMultiLineComment)

parseComment :: Parser Comment
parseComment =
  try parseSingleLineComment <|> try parseMultiLineComment

parseSingleLineComment :: Parser Comment
parseSingleLineComment =
  string "//" *> manyTill anyChar (try eol)

parseMultiLineComment :: Parser Comment
parseMultiLineComment =
  string "/*" *> manyTill anyChar (try (string "*/"))
