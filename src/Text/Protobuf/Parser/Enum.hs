module Text.Protobuf.Parser.Enum
  ( enum,
    parseEnum',
  )
where

import Text.Parsec hiding (option)
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Boolean
import Text.Protobuf.Parser.LexicalElement.Constant
import Text.Protobuf.Parser.LexicalElement.EmptyStatement
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.IntegerLiteral
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Parser.LexicalElement.StringLiteral
import Text.Protobuf.Parser.Option
import Text.Protobuf.Parser.Reserved
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

parseEnum' :: Protobuf -> Parser Protobuf
parseEnum' p = do
  x <- enum
  return
    ( Text.Protobuf.Types.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [], enums = [x], messages = [], services = []})
    )

-- enum = "enum" enumName enumBody
enum :: Parser Text.Protobuf.Types.Enum
enum =
  Text.Protobuf.Types.Enum
    <$> ( spaces'
            *> string "enum"
            *> spaces1
            *> enumName
        )
    <*> ( spaces'
            *> char '{'
            *> spaces'
            *> fields
            <* spaces'
            <* char '}'
            <* spaces'
        )
  where
    fields = try enumField `sepEndBy1` char ';'

-- enumBody = "{" { option | enumField | emptyStatement | reserved } "}"
enumBody :: Parser [EnumField]
enumBody =
  spaces'
    *> char '{'
    *> spaces'
    *> (many (try (EnumReserved <$> reserved) <|> try enumField <|> try (EnumOption <$> option) <|> try (emptyStatement >> return EnumEmptyStatement)))
    <* spaces'
    <* char '}'
    <* spaces'

-- enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
enumField :: Parser EnumField
enumField =
  EnumValue
    <$> ident
    <* char '='
    <* spaces' -- TODO: (-)
    <*> intLit
    <*> (try enumValueOption `sepBy` char ',' <|> return [])

-- enumValueOption = optionName "=" constant
enumValueOption :: Parser FieldOption
enumValueOption =
  FieldOption
    <$> ( spaces'
            *> optionName
            <* spaces'
            <* char '='
            <* spaces'
        )
    <*> ( spaces'
            *> constant
            <* spaces'
        )
