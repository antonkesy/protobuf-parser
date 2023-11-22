module ProtoParser.Package (parsePackage, parsePackage') where

import ProtoParser.Misc (spaces1)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parsePackage' :: Parser Protobuf
parsePackage' = do
  package' <- parsePackage
  return (Protobuf {package = [package'], imports = [], options = [], enums = [], messages = [], services = []})

parsePackage :: Parser Package
parsePackage = do
  skipMany space
  _ <- string "package" <?> "Expected package keyword"
  spaces1
  anyChar `manyTill` char ';' <?> "Expected package name followed by ';'"
