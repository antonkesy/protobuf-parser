module ProtoParser.Package (parsePackage, parsePackage') where

import qualified Data.Maybe
import ProtoParser.Space (spaces', spaces1)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parsePackage' :: Protobuf -> Parser Protobuf
parsePackage' p = do
  package' <- parsePackage
  if Data.Maybe.isJust (package p)
    then unexpected ": There can only be one package definition per file"
    else
      return
        ( Protobuf.merge
            p
            (Protobuf {package = Just package', imports = [], options = [], enums = [], messages = [], services = []})
        )

parsePackage :: Parser Package
parsePackage = do
  spaces'
  _ <- string "package" <?> "Expected package keyword"
  spaces1
  anyChar `manyTill` char ';' <?> "Expected package name followed by ';'"
