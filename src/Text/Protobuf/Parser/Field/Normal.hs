module Text.Protobuf.Parser.Field.Normal (field, fieldOptions, fieldOption) where

import Text.Parsec hiding (option)
import Text.Parsec.String
import Text.Protobuf.Parser.Field
import Text.Protobuf.Parser.LexicalElement.Boolean
import Text.Protobuf.Parser.LexicalElement.Constant
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Parser.Option
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

-- field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
field :: Parser Field
field =
  NormalField
    <$> ( spaces'
            *> (try (string "repeated" >> return True) <|> return False)
            <* spaces'
        )
    <*> ( spaces'
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

-- fieldOptions = fieldOption { ","  fieldOption }
fieldOptions :: Parser [FieldOption]
fieldOptions = try (fieldOption `sepBy1` char ',')

-- fieldOption = optionName "=" constant
fieldOption :: Parser FieldOption
fieldOption =
  FieldOption
    <$> ( spaces'
            *> optionName
            <* spaces'
        )
    <*> ( spaces'
            *> char '='
            *> spaces'
            *> constant
            <* spaces'
        )
