module ProtoParser.Comment
  ( parseComment,
    parseSingleLineComment,
    parseMultiLineComment,
  )
where

import ProtoParser.Misc (eol)
import Protobuf (Comment)
import Text.Parsec
import Text.Parsec.String

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
