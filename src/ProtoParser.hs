-- TODO: export only the necessary functions
module ProtoParser
  ( module ProtoParser.Enum,
    module ProtoParser.Misc,
    module ProtoParser.Import,
    parseProtobuf,
  )
where

import ProtoParser.Enum
import ProtoParser.Import
import ProtoParser.Misc
import ProtoParser.Comment
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseProtobuf :: String -> (Either ParseError Protobuf)
parseProtobuf = parse parseProtobuf' ""

parseProtobuf' :: Parser Protobuf
parseProtobuf' = do
  _x <- parseImport
  -- TODO: how to add multiple parser outputs?
  return
    ( Protobuf
        { package = "",
          imports = [],
          options = [],
          enums = [],
          messages = [],
          services = []
        }
    )
