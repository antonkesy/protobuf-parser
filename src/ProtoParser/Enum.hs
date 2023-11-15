module ProtoParser.Enum
  ( protoEnum,
    enumField,
    enumNumber,
    protoName,
    reservedNumbers,
  )
where

import Data.Maybe (catMaybes)
import Debug.Trace
import ProtoParser.Misc
import Protobuf
import Text.Parsec
import Text.Parsec.String

protoEnum :: Parser Protobuf.Enum
protoEnum = do
  skipMany space
  _ <- string "enum" <?> "Expected enum keyword"
  skipMany space
  name <- protoName <?> "Expected enum name"
  whitespace
  _ <- char '{' <?> ("Expected '{' after enum name" ++ name)
  skipMany space
  values <- enumField `sepEndBy1` char ';'
  if null (catMaybes values)
    then fail "Expected at least one enum value"
    else do
      whitespace
      _ <- char '}'
      -- TODO: check enum values for correctness (numbers and names) -> in extra function traversing finished proto data
      return (Protobuf.Enum name (catMaybes values))

enumField :: Parser (Maybe EnumField)
enumField = do
  skipMany space
  isEnd <- option False (lookAhead (char '}') >> return True)
  if isEnd
    then return Nothing
    else do
      name <- protoName
      case name of
        "option" -> do enumOption
        "reserved" -> do enumReserved
        _ -> do
          skipMany space
          _ <- char '='
          skipMany space
          number <- enumNumber
          skipMany space
          return (Just (EnumValue name number))

-- https://protobuf.dev/programming-guides/proto3/#enum
enumOption :: Parser (Maybe EnumField)
enumOption = do
  whitespace
  optionName <- protoName
  case optionName of
    "allow_alias" -> do
      whitespace
      _ <- char '='
      whitespace
      active <- try (string "true" >> return True) <|> (string "false" >> return False) <?> "Expected true or false"
      whitespace
      return (Just (EnumOption "allow_alias" active))
    _ -> fail "Unknown option"

-- https://protobuf.dev/programming-guides/proto3/#reserved
enumReserved :: Parser (Maybe EnumField)
enumReserved = do
  whitespace
  reservedValues <- (try reservedNames <|> try reservedNumbers) `sepEndBy` char ','
  isParsedCorrect <- option True (try (lookAhead enumNumber) >> return False) <|> (try (lookAhead protoName) >> return False)
  if not isParsedCorrect
    then fail "Expected either numbers or names, end of enum or separator"
    else case reservedValues of
      [] -> fail "Expected at least one reserved value (either number or name)"
      _ -> do
        let numbers =
              [ case x of
                  Numbers l -> l
                  _ -> []
                | x <- reservedValues
              ]
            names =
              [ case x of
                  Names n -> n
                  _ -> []
                | x <- reservedValues
              ]
        if not (all null names) && not (all null numbers)
          then fail "Expected either numbers or names, not both"
          else
            if all null numbers
              then
                if all null names
                  then fail "Expected either numbers or names"
                  else return (Just (EnumReserved (Names (concat names))))
              else return (Just (EnumReserved (Numbers (concat numbers))))

reservedNames :: Parser EnumReservedValues
reservedNames = do
  _ <- many space
  _ <- char '\"'
  name <- protoName
  _ <- char '\"'
  return (Names [name])

reservedNumbersSingle :: Parser EnumReservedValues
reservedNumbersSingle = do
  _ <- many space
  firstNumber <- enumNumber
  _ <- many space
  return (Numbers [firstNumber])

reservedNumbersRange :: Parser EnumReservedValues
reservedNumbersRange = do
  let numValues = try enumNumber <|> try (string "min" >> return 0) <|> try (string "max" >> return 0xFFFFFFFF)
  firstNumber <- numValues
  _ <- many space
  _ <- string "to"
  _ <- many space
  secondNumber <- numValues
  return (Numbers [firstNumber .. secondNumber])

reservedNumbers :: Parser EnumReservedValues
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
