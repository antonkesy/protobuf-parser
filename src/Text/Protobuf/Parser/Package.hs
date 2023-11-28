module Text.Protobuf.Parser.Package (Text.Protobuf.Types.package, parsePackage') where

import qualified Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Types

parsePackage' :: Protobuf -> Parser Protobuf
parsePackage' p = do
  package' <- Text.Protobuf.Parser.Package.package
  if Data.Maybe.isJust (Text.Protobuf.Types.package p)
    then unexpected ": There can only be one package definition per file"
    else
      return
        ( Text.Protobuf.Types.merge
            p
            (Protobuf {syntax = Nothing, Text.Protobuf.Types.package = Just package', imports = [], options = [], enums = [], messages = [], services = []})
        )

-- package = "package" fullIdent ";"
package :: Parser Package
package = do
  spaces'
    *> string "package"
    *> spaces1
    *> fullIdent
    <* char ';'
    <* spaces'
