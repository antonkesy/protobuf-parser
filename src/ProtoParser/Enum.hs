module ProtoParser.Enum
  ( protoEnum,
    enumField,
    enumNumber,
    protoName,
    reservedNumbers,
  )
where

import Debug.Trace
import ProtoParser.Misc
import Protobuf
import Text.Parsec
import Text.Parsec.String

protoEnum :: Parser Protobuf.Enum
protoEnum = do
  spaces
  _ <- string "enum" <?> "Expected enum keyword"
  spaces
  name <- protoName <?> "Expected enum name"
  spaces
  _ <- char '{' <?> ("Expected '{' after enum name" ++ name)
  spaces
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
      spaces -- TODO: 1 space required?
      _ <- char '='
      spaces -- TODO: 1 space required?
      number <- enumNumber
      spaces
      return (EnumValue name number)

-- https://protobuf.dev/programming-guides/proto3/#enum
enumOption :: Parser EnumField
enumOption = do
  spaces
  optionName <- protoName
  case optionName of
    "allow_alias" -> do
      spaces
      _ <- char '='
      spaces
      active <- parseBoolOption
      spaces
      return (EnumOption "allow_alias" active)
    _ -> fail "Unknown option"

parseBoolOption :: Parser Bool
parseBoolOption =
  try (string "true" >> return True)
    <|> (string "false" >> return False)
    <?> "Expected true or false"

-- https://protobuf.dev/programming-guides/proto3/#reserved
enumReserved :: Parser EnumField
enumReserved = do
  spaces
  try parseReservedNames <|> try parseReservedNumbers

parseReservedNames :: Parser EnumField
parseReservedNames = do
  names <- try reservedNames `sepEndBy1` char ','
  return (EnumReserved (Names (concat names)))

parseReservedNumbers :: Parser EnumField
parseReservedNumbers = do
  numbers <- try reservedNumbers `sepEndBy1` char ','
  return (EnumReserved (Numbers (concat numbers)))

reservedNames :: Parser [EnumName]
reservedNames = do
  _ <- many space
  _ <- char '\"'
  name <- protoName
  _ <- char '\"'
  return [name]

reservedNumbersSingle :: Parser [EnumNumber]
reservedNumbersSingle = do
  _ <- many space
  firstNumber <- enumNumber
  _ <- many space
  return [firstNumber]

reservedNumbersRange :: Parser [EnumNumber]
reservedNumbersRange = do
  let numValues = try enumNumber <|> try (string "min" >> return 0) <|> try (string "max" >> return 0xFFFFFFFF)
  firstNumber <- numValues
  _ <- many space
  _ <- string "to"
  _ <- many space
  secondNumber <- numValues
  return [firstNumber .. secondNumber]

reservedNumbers :: Parser [EnumNumber]
reservedNumbers = try reservedNumbersRange <|> try reservedNumbersSingle

enumNumber :: Parser EnumNumber
enumNumber =
  -- https://protobuf.dev/programming-guides/proto3/#enum
  let val = (read <$> many1 digit)
   in do
        n <- val
        if n >= (minBound :: FieldNumber) && n <= (maxBound :: FieldNumber)
          then return (fromIntegral n)
          else fail "Number not in valid range"
