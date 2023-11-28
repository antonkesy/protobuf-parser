module Text.Protobuf.Parser.Import (import', parseImport') where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Parser.LexicalElement.StringLiteral
import Text.Protobuf.Types

parseImport' :: Protobuf -> Parser Protobuf
parseImport' p = do
  imp <- import'
  return
    ( Text.Protobuf.Types.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [imp], options = [], enums = [], messages = [], services = []})
    )

pathExtension :: String
pathExtension = ".proto"

-- import = "import" [ "weak" | "public" ] strLit ";"
import' :: Parser ImportPath
import' = do
  spaces'
    *> string "import"
    *> spaces1
    *> (char '"' <?> "Expected '\"' after import keyword")
  path <- strLit <* char ';'

  return (path ++ pathExtension)
