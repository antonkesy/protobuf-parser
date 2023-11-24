module ProtoParser.Enum
  ( protoEnum,
    parseEnum',
    enumField,
    enumNumber,
    enumNumberRange,
    protoName,
    reservedNumbers,
  )
where

import ProtoParser.Reserved
import ProtoParser.Space (spaces')
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseEnum' :: Protobuf -> Parser Protobuf
parseEnum' p = do
  x <- protoEnum
  return
    ( Protobuf.merge
        p
        (Protobuf {package = Nothing, imports = [], options = [], enums = [x], messages = [], services = []})
    )

protoEnum :: Parser Protobuf.Enum
protoEnum = do
  spaces'
  _ <- string "enum" <?> "Expected enum keyword"
  spaces'
  name <- protoName <?> "Expected enum name"
  spaces'
  _ <- char '{' <?> ("Expected '{' after enum name" ++ name)
  spaces'
  values <- try enumField `sepEndBy1` (try (string ";") <|> try (string ";\n")) <?> "Expected at least one enum value"
  return (Protobuf.Enum name values)

enumField :: Parser EnumField
enumField = do
  spaces
  name <- protoName
  case name of
    "option" -> do enumOption
    "reserved" -> do enumReserved
    _ -> do
      spaces'
      _ <- char '='
      spaces'
      number <- enumNumber
      spaces'
      return (EnumValue name number)

-- https://protobuf.dev/programming-guides/proto3/#enum
enumOption :: Parser EnumField
enumOption = do
  spaces
  optionName <- protoName
  case optionName of
    "allow_alias" -> do
      spaces'
      _ <- char '='
      spaces'
      active <- parseBoolOption
      spaces'
      return (EnumOption "allow_alias" active)
    _ -> fail "Unknown option"

parseBoolOption :: Parser Bool
parseBoolOption =
  try (string "true" >> return True)
    <|> (string "false" >> return False)
    <?> "Expected true or false"

enumReserved :: Parser EnumField
enumReserved = do
  spaces'
  try parseReservedNames <|> try parseReservedNumbers

parseReservedNames :: Parser EnumField
parseReservedNames = do
  names <- reservedNames
  return (EnumReserved (ReservedEnumNames names))

parseReservedNumbers :: Parser EnumField
parseReservedNumbers = do
  numbers <- try (reservedNumbers enumNumber enumNumberRange) `sepEndBy1` char ','
  return (EnumReserved (ReservedEnumNumbers (concat numbers)))

enumNumber :: Parser EnumNumber
enumNumber =
  -- https://protobuf.dev/programming-guides/proto3/#enum
  let val = (read <$> many1 digit)
   in do
        -- TODO move min/max to here but in seperate parser because cant mix with standalone
        n <- val
        if n >= (minBound :: EnumNumber) && n <= (maxBound :: EnumNumber)
          then return n
          else fail "Number not in valid range"

enumNumberRange :: Parser EnumNumber
enumNumberRange = do
  n <- enumNumber <|> try (string "min" >> return 0) <|> try (string "max" >> return 0xFFFFFFFF)
  return n
