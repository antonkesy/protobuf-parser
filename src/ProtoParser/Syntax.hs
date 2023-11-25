module ProtoParser.Syntax (parseSyntax, parseSyntax') where

import qualified Data.Maybe
import ProtoParser.Space (spaces')
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseSyntax' :: Protobuf -> Parser Protobuf
parseSyntax' p = do
  syn <- parseSyntax
  if Data.Maybe.isJust (syntax p)
    then unexpected ": There can only be one syntax definition per file"
    else
      return
        ( Protobuf.merge
            p
            (Protobuf {syntax = Just syn, package = Nothing, imports = [], options = [], enums = [], messages = [], services = []})
        )

parseSyntax :: Parser Syntax
parseSyntax = do
  spaces'
  _ <- string "syntax" <?> "Expected 'syntax' keyword"
  spaces'
  _ <- char '=' <?> "Expected '=' after 'syntax' keyword"
  spaces'
  _ <- char '"' <?> "Expected '\"' after 'syntax' keyword"
  syn <-
    try (string "proto2" >> return Proto2)
      <|> try (string "proto3" >> return Proto3)
  spaces'
  _ <- char '"' <?> "Expected '\"' after 'syntax' value"
  _ <- char ';' <?> "Expected ';' at end of syntax statement"
  return syn
