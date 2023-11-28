module Text.Protobuf.Parser.Field.Map () where

import Text.Parsec hiding (option)
import Text.Parsec.String
import Text.Protobuf.Parser.Field
import Text.Protobuf.Parser.Field.Normal
import Text.Protobuf.Parser.LexicalElement.Boolean
import Text.Protobuf.Parser.LexicalElement.Constant
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Parser.Option
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

-- mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
mapField :: Parser Field
mapField =
  MapField
    <$> (spaces' *> string "map" *> spaces1 *> char '<' *> spaces' *> keyType)
    <*> (spaces' *> char ',' *> spaces' *> type')
    <*> (spaces' *> char '>' *> spaces1 *> mapName)
    <*> (spaces' *> char '=' *> spaces' *> fieldNumber)
    <*> (spaces' *> (try fieldOptions <|> return []) <* spaces' <* char ';')

-- keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
-- "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
keyType :: Parser MapKeyType
keyType =
  ( try (string "int32")
      <|> try (string "int64")
      <|> try (string "uint32")
      <|> try (string "uint64")
      <|> try (string "sint32")
      <|> try (string "sint64")
      <|> try (string "fixed32")
      <|> try (string "fixed64")
      <|> try (string "sfixed32")
      <|> try (string "sfixed64")
      <|> try (string "bool")
      <|> try (string "string")
  )
