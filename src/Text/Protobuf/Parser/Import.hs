module Text.Protobuf.Parser.Import (parseImport, parseImport') where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Space (spaces', spaces1)
import Text.Protobuf.Types

parseImport' :: Protobuf -> Parser Protobuf
parseImport' p = do
  imp <- parseImport
  return
    ( Text.Protobuf.Types.merge
        p
        ( Protobuf
            { syntax = Nothing,
              package = Nothing,
              imports = [imp],
              options = [],
              enums = [],
              messages = [],
              services = []
            }
        )
    )

pathExtension :: String
pathExtension = ".proto"

parseImport :: Parser ImportPath
parseImport =
  spaces'
    *> (string "import" <?> "Expected import keyword")
    *> spaces1
    *> (char '"' <?> "Expected '\"' after import keyword")
    *> ( (++ pathExtension)
           <$> (anyChar `manyTill` string (pathExtension ++ "\""))
       )
    <* spaces'
    <* char ';'
