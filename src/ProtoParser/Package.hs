module ProtoParser.Package (parsePackage) where

import ProtoParser.Misc (spaces1)
import Protobuf (Package)
import Text.Parsec
import Text.Parsec.String

parsePackage :: Parser Package
parsePackage = do
  skipMany space
  _ <- string "package" <?> "Expected package keyword"
  spaces1
  anyChar `manyTill` char ';' <?> "Expected package name followed by ';'"
