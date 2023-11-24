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

import Debug.Trace
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
protoValue' o = do
  x <-
    choice
      [ try parsePackage',
        try parseImport',
        try parseComment',
        try parseEnum',
        try parseMessage'
      ]
  isEnd <- try ((lookAhead anyToken) >> return False) <|> return True
  if isEnd
    then return (merge o x)
    else do
      y <- protoValue' (merge o x)
      return y
