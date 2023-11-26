module Text.Protobuf
  ( parseProtobuf,
    parseProtoFile,
  )
where

-- TODO: rename to Text.Protobuf.Parser

import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Comment
import Text.Protobuf.Parser.Enum
import Text.Protobuf.Parser.Import
import Text.Protobuf.Parser.Message
import Text.Protobuf.Parser.Option
import Text.Protobuf.Parser.Package
import Text.Protobuf.Parser.Service
import Text.Protobuf.Parser.Syntax
import Text.Protobuf.Types

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
