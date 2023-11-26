module ProtoParser
  ( parseProtobuf,
    parseProtoFile,
  )
where

import ProtoParser.Comment
import ProtoParser.Enum
import ProtoParser.Import
import ProtoParser.Message
import ProtoParser.Option
import ProtoParser.Package
import ProtoParser.Service
import ProtoParser.Syntax
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

-- TODO: extend - https://protobuf.dev/programming-guides/proto3/#option-targets
-- TODO: rework according to https://protobuf.dev/reference/protobuf/proto3-spec/
protoValue' :: Protobuf -> Parser Protobuf
protoValue' old =
  ( try (parsePackage' old)
      <|> try (parseImport' old)
      <|> try (parseComment' old)
      <|> try (parseEnum' old)
      <|> try (parseMessage' old)
      <|> try (parseOption' old)
      <|> try (parseSyntax' old)
      <|> try (parseService' old)
  )
    >>= \new ->
      (notFollowedBy anyChar >> return new)
        <|> protoValue' new
