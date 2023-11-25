module ProtoParser
  ( module ProtoParser.Enum,
    module ProtoParser.Type,
    module ProtoParser.Import,
    module ProtoParser.Comment,
    module ProtoParser.Message,
    module ProtoParser.Package,
    module ProtoParser.Service,
    module ProtoParser.EndOfLine,
    module ProtoParser.Syntax,
    module ProtoParser.Option,
    parseProtobuf,
    parseProtoFile,
  )
where

import ProtoParser.Comment
import ProtoParser.EndOfLine
import ProtoParser.Enum
import ProtoParser.Import
import ProtoParser.Message
import ProtoParser.Option
import ProtoParser.Package
import ProtoParser.Service
import ProtoParser.Syntax
import ProtoParser.Type
import Protobuf
import System.IO
import Text.Parsec
import Text.Parsec.String

parseProtobuf :: String -> Either ParseError Protobuf
parseProtobuf = parse protoValue ""

parseProtoFile :: FilePath -> IO (Either ParseError Protobuf)
parseProtoFile filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  -- hClose handle
  return (parse protoValue filePath contents)

protoValue :: Parser Protobuf
protoValue = do
  protoValue' emptyProtobuf

protoValue' :: Protobuf -> Parser Protobuf
protoValue' old = do
  new <-
    choice
      [ try (parsePackage' old),
        try (parseImport' old),
        try (parseComment' old),
        try (parseEnum' old),
        try (parseMessage' old),
        try (parseOption' old),
        try (parseSyntax' old),
        try (parseService' old)
      ]
  isEnd <- try (lookAhead anyChar >> return False) <|> return True
  if isEnd
    then do
      return new
    else do
      protoValue' new
