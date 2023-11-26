module ProtoParser.Option (parseOption, parseOption', parseFieldOption) where

import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type (parseBool, parseCustomName, parseString, protoName)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseOption' :: Protobuf -> Parser Protobuf
parseOption' p = do
  opt <- parseOption
  return
    ( Protobuf.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [opt], enums = [], messages = [], services = []})
    )

-- https://protobuf.dev/programming-guides/proto3/#options
-- TODO: value can be bool, string, protoname until ';'

parseOption :: Parser Option
parseOption =
  Option
    <$> ( spaces'
            *> string "option"
            *> spaces1
            *> protoName
            <* spaces1
        )
    <*> ( spaces'
            *> char '='
            *> spaces'
            *> ( (StringValue <$> try parseString)
                   <|> (BoolValue <$> try parseBool)
                   <|> (CompoundValue <$> try protoName)
               )
            <* spaces'
            <* char ';'
        )

parseFieldOption :: Parser [FieldOption]
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
        *> ( (StringValue <$> try parseString)
               <|> (BoolValue <$> try parseBool)
               <|> (CompoundValue <$> try protoName)
           )
        <* spaces'
    singleFieldOption =
      FieldOption
        <$> name
        <*> value
