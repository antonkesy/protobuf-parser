-- TODO: export only the necessary functions
module ProtoParser
  ( module ProtoParser.Enum,
    module ProtoParser.Misc,
    module ProtoParser.Import,
    module ProtoParser.Comment,
    module ProtoParser.Message,
    module ProtoParser.Package,
    module ProtoParser.Service,
    parseProtobuf,
  )
where

import ProtoParser.Comment
import ProtoParser.Enum
import ProtoParser.Import
import ProtoParser.Message
import ProtoParser.Misc
import ProtoParser.Package
import ProtoParser.Service
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseProtobuf :: String -> Either ParseError Protobuf
parseProtobuf = parse protoValue ""

protoValue :: Parser Protobuf
protoValue = do
  x <- (protoValue' emptyProtobuf)
  return x

protoValue' :: Protobuf -> Parser Protobuf
protoValue' old = do
  new <-
    choice
      [ try (parsePackage' old),
        try (parseImport' old),
        try (parseComment' old),
        try (parseEnum' old),
        try (parseMessage' old)
      ]
  isEnd <- try ((lookAhead anyToken) >> return False) <|> return True
  if isEnd
    then return new
    else do
      newNew <- protoValue' new
      return newNew
