module Text.Protobuf.Parser
  ( parseProtobuf,
    parseProtoFile,
  )
where

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
  contents <- readFile filePath
  return (parse protoValue filePath contents)

protoValue :: Parser Protobuf
protoValue = do
  protoValue' emptyProtobuf

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
