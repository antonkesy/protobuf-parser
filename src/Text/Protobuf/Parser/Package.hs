module Text.Protobuf.Parser.Package (parsePackage, parsePackage') where

import qualified Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Space (spaces', spaces1)
import Text.Protobuf.Types

parsePackage' :: Protobuf -> Parser Protobuf
parsePackage' p = do
  package' <- parsePackage
  if Data.Maybe.isJust (package p)
    then unexpected ": There can only be one package definition per file"
    else
      return
        ( Text.Protobuf.Types.merge
            p
            (Protobuf {syntax = Nothing, package = Just package', imports = [], options = [], enums = [], messages = [], services = []})
        )

parsePackage :: Parser Package
parsePackage = do
  spaces'
    *> string "package"
    *> spaces1
    *> (anyChar `manyTill` char ';')
    <* spaces'
