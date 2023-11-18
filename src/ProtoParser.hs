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
parseProtobuf = parse parseProtobuf' ""

-- TODO : check for too many ';'

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
