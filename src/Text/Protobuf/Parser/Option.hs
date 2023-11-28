module Text.Protobuf.Parser.Option (option, optionName, parseOption', parseFieldOption) where

import Text.Parsec hiding (option)
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Boolean
import Text.Protobuf.Parser.LexicalElement.Constant
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Parser.Type (parseCustomName, parseString, protoName)
import Text.Protobuf.Types

parseOption' :: Protobuf -> Parser Protobuf
parseOption' p = do
  opt <- option
  return
    ( Text.Protobuf.Types.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [opt], enums = [], messages = [], services = []})
    )

-- https://protobuf.dev/programming-guides/proto3/#options
-- TODO: value can be bool, string, protoname until ';'

-- option = "option" optionName  "=" constant ";"
option :: Parser Option
option =
  Option
    <$> ( spaces'
            *> string "option"
            *> spaces1
            *> optionName
            <* spaces1
        )
    <*> ( spaces'
            *> char '='
            *> spaces'
            *> constant
            <* spaces'
            <* char ';'
        )

-- optionName = ( ident | "(" ["."] fullIdent ")" )
optionName :: Parser String
optionName = do
  -- TODO
  try ident
    <|> try (char '(' *> char '.' *> fullIdent <* char ')')
    <|> try (char '(' *> fullIdent <* char ')')

parseFieldOption :: Parser [Option]
parseFieldOption =
  start
    *> try ((try singleFieldOption `sepBy1` try (char ',')) <* end)
  where
    start = spaces' *> char '[' *> spaces'
    end = spaces' <* char ']'
    name = spaces' *> (try protoName <|> parseCustomName) <* spaces'
    value =
      spaces'
        *> char '='
        *> spaces'
        *> constant
        <* spaces'
    singleFieldOption =
      Option
        <$> name
        <*> value
