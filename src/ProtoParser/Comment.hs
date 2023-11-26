module ProtoParser.Comment
  ( parseComment,
    parseComment',
    parseSingleLineComment,
    parseMultiLineComment,
    removeComment,
  )
where

import Control.Monad (void)
import ProtoParser.EndOfLine (eol)
import Protobuf
import Text.Parsec
import Text.Parsec.String

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
