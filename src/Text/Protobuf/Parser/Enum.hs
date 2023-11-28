module Text.Protobuf.Parser.Enum
  ( parseEnum,
    parseEnum',
    parseEnumField,
    enumNumber,
    enumNumberRange,
    protoName,
    reservedNumbers,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Option
import Text.Protobuf.Parser.Reserved
import Text.Protobuf.Parser.Space (spaces', spaces1)
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

parseEnum' :: Protobuf -> Parser Protobuf
parseEnum' p = do
  x <- parseEnum
  return
    ( Text.Protobuf.Types.merge
        p
        ( Protobuf
            { syntax = Nothing,
              package = Nothing,
              imports = [],
              options = [],
              enums = [x],
              messages = [],
              services = []
            }
        )
    )

parseEnum :: Parser Text.Protobuf.Types.Enum
parseEnum =
  Text.Protobuf.Types.Enum
    <$> ( spaces'
            *> string "enum"
            *> spaces1
            *> name
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
    name = protoName
    fields = try parseEnumField `sepEndBy1` char ';'

parseEnumField :: Parser EnumField
parseEnumField =
  spaces' *> (try reservedField <|> try optionField <|> try valueField)
  where
    fieldName = spaces' *> protoName
    fieldNumber = spaces' *> char '=' *> spaces' *> enumNumber
    reservedValues =
      try (ReservedEnumNames <$> reservedNames)
        <|> try
          ( ReservedEnumNumbers
              <$> reservedNumbers enumNumber enumNumberRange
          )
    valueField =
      EnumValue
        <$> fieldName
        <*> fieldNumber
        <*> (try parseFieldOption <|> return [])
    optionField = EnumOption <$> parseOption
    reservedField =
      EnumReserved
        <$> (string "reserved" *> spaces' *> reservedValues)

enumNumber :: Parser EnumNumber
enumNumber =
  do
    n <- read <$> many1 digit
    if n >= (minBound :: EnumNumber) && n <= (maxBound :: EnumNumber)
      then return n
      else fail "Number not in valid range"

enumNumberRange :: Parser EnumNumber
enumNumberRange =
  enumNumber
    <|> try (string "min" >> return 0)
    <|> try (string "max" >> return 0xFFFFFFFF)
