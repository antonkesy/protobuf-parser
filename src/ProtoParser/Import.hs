module ProtoParser.Import (parseImport) where

import ProtoParser.Misc (spaces1)
import Protobuf (ImportPath)
import Text.Parsec
import Text.Parsec.String

pathExtension :: String
pathExtension = ".proto"

parseImport :: Parser ImportPath
parseImport = do
  skipMany space
  _ <- string "import" <?> "Expected import keyword"
  spaces1
  _ <- char '"' <?> "Expected '\"' after import keyword"
  path <- anyChar `manyTill` string (pathExtension ++ "\"")
  spaces
  _ <- char ';' <?> "Expected ';' at end of import statement"
  return (path ++ pathExtension)
