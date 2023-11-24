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
        (Protobuf {package = Nothing, imports = [imp], options = [], enums = [], messages = [], services = []})
    )

pathExtension :: String
pathExtension = ".proto"

parseImport :: Parser ImportPath
parseImport = do
  spaces'
  _ <- string "import" <?> "Expected import keyword"
  spaces1
  _ <- char '"' <?> "Expected '\"' after import keyword"
  path <- anyChar `manyTill` string (pathExtension ++ "\"")
  spaces'
  _ <- char ';' <?> "Expected ';' at end of import statement"
  return (path ++ pathExtension)
