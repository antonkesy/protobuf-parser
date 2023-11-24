module ProtoParser.Package (parsePackage, parsePackage') where

import ProtoParser.Misc (spaces1)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parsePackage' :: Protobuf -> Parser Protobuf
parsePackage' p = do
  package' <- parsePackage
  if package p /= Nothing
    then unexpected ": There can only be one package definition per file"
    else
      return
        ( Protobuf.merge
            p
            (Protobuf {package = (Just package'), imports = [], options = [], enums = [], messages = [], services = []})
        )

parsePackage :: Parser Package
parsePackage = do
  skipMany space
  _ <- string "package" <?> "Expected package keyword"
  spaces1
  anyChar `manyTill` char ';' <?> "Expected package name followed by ';'"
