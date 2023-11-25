module ProtoParser.Option (parseOption, parseOption') where

import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type (parseBool, parseString, protoName)
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

-- TODO: post options -> [deprecated = true], [packed = false] ...,,,
-- TODO: split by ',' and same possible values as protoOptions

-- [(string_name) = ...]; <- valid
-- [retention = RETENTION_SOURCE];
-- TODO: [retention = RETENTION_SOURCE, retention = RETENTION_SOURCE];  <- how to set this in data type?
-- parseFieldOption :: Parser FieldOption
-- parseFieldOption =
--   FieldOption
--     <$> ( spaces'
--             *> char '['
--             *> spaces'
--             *> protoName
--             <* spaces'
--         )
--     <*> ( spaces'
--             *> char '='
--             *> spaces'
--             *> ( (StringValue <$> try parseString)
--                    <|> (BoolValue <$> try parseBool)
--                    <|> (CompoundValue <$> try protoName)
--                )
--             <* spaces'
--             <* char ']'
--             <* char ';'
--         )
