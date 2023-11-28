module Text.Protobuf.Parser.Field.OneOf () where

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

-- oneof = "oneof" oneofName "{" { option | oneofField } "}"
oneof :: Parser Field
oneof =
  OneOf
    <$> ( spaces'
            *> string "oneof"
            *> spaces1
            *> oneofName
            <* spaces'
        )
    <*> (spaces' *> char '{' *> spaces' *> fields <* spaces' <* char '}' <* spaces')
  where
    fields = ((Right <$> try oneofField) <|> (Left <$> try option)) `sepEndBy` char ';'

-- oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
oneofField :: Parser Field
oneofField =
  OneOfField
    <$> ( spaces'
            *> type'
            <* spaces'
        )
    <*> ( spaces'
            *> fieldName
            <* spaces'
        )
    <*> ( spaces'
            *> char '='
            *> spaces'
            *> fieldNumber
            <* spaces'
        )
    <*> ( spaces'
            *> fieldOptions
            <* spaces'
            <* char ';'
        )
