module ProtoParser.Import (parseImport, parseImport') where

import ProtoParser.Space (spaces', spaces1)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseImport' :: Protobuf -> Parser Protobuf
parseImport' p = do
  imp <- parseImport
  return
    ( Protobuf.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [imp], options = [], enums = [], messages = [], services = []})
    )

pathExtension :: String
pathExtension = ".proto"

parseImport :: Parser ImportPath
parseImport =
  spaces'
    *> (string "import" <?> "Expected import keyword")
    *> spaces1
    *> (char '"' <?> "Expected '\"' after import keyword")
    *> ((++ pathExtension) <$> (anyChar `manyTill` string (pathExtension ++ "\"")))
    <* spaces'
    <* char ';'
