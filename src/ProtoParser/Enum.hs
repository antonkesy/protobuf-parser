module ProtoParser.Enum
  ( parseEnum,
    parseEnum',
    parseEnumField,
    enumNumber,
    enumNumberRange,
    protoName,
    reservedNumbers,
  )
where

import ProtoParser.Option
import ProtoParser.Reserved
import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseEnum' :: Protobuf -> Parser Protobuf
parseEnum' p = do
  x <- parseEnum
  return
    ( Protobuf.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [], enums = [x], messages = [], services = []})
    )

parseEnum :: Parser Protobuf.Enum
parseEnum =
  Protobuf.Enum
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
        <|> try (ReservedEnumNumbers <$> reservedNumbers enumNumber enumNumberRange)
    valueField =
      EnumValue
        <$> fieldName
        <*> fieldNumber
        <*> (try parseFieldOption <|> return [])
    optionField =
      EnumOption
        <$> (string "option" *> spaces1 *> protoName)
        <*> (spaces1 *> char '=' *> spaces' *> parseBool)
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
