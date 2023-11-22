-- TODO: export only the necessary functions
module ProtoParser
  ( module ProtoParser.Enum,
    module ProtoParser.Misc,
    module ProtoParser.Import,
    module ProtoParser.Comment,
    module ProtoParser.Message,
    module ProtoParser.Package,
    parseProtobuf,
  )
where

import Debug.Trace
import ProtoParser.Comment
import ProtoParser.Enum
import ProtoParser.Import
import ProtoParser.Message
import ProtoParser.Misc
import ProtoParser.Package
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseProtobuf :: String -> Either ParseError Protobuf
parseProtobuf = parse protoValue ""

-- TODO : check for too many ';'

protoValue :: Parser Protobuf
protoValue = do
  x <-
    choice
      [ try parsePackage',
        try parseImport',
        try parseComment',
        try parseEnum',
        try parseMessage'
      ]
      `sepBy1` lookAhead anyToken
  return (merge' x)
