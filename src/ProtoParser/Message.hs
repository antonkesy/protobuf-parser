module ProtoParser.Message (parseMessage, parseMessage', parseMap) where

import ProtoParser.Reserved
import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseMessage' :: Protobuf -> Parser Protobuf
parseMessage' p = do
  x <- parseMessage
  -- TODO: check for validity of message?
  return
    ( Protobuf.merge
        p
        (Protobuf {package = Nothing, imports = [], options = [], enums = [], messages = [x], services = []})
    )

parseMessage :: Parser Message
parseMessage = do
  parseMessage''

parseMessage'' :: Parser Message
parseMessage'' = do
  spaces'
  _ <- string "message"
  spaces1
  name <- protoName
  spaces'
  _ <- char '{'
  spaces'
  fields <- parseMessageField `sepEndBy` char ';'
  spaces'
  _ <- char '}'
  return (Message name (fields))

parseMessageField :: Parser MessageField
parseMessageField = do
  spaces'
  -- _ <- try (string "repeated") -- TODO: optional
  -- spaces'
  -- TODO: extra function parser for reserved to avoid this workaround
  t <- parseDataType
  case t of
    (Compound "reserved") -> do messageReserved
    _ -> do
      spaces'
      name <- protoName
      spaces'
      _ <- char '='
      spaces'
      fieldNumber <- protoNumber
      spaces'
      return (MessageField t name fieldNumber False)

parseDataType :: Parser DataType
parseDataType =
  do
    Scalar <$> parseScalarType
    <|> Compound <$> protoName

----------------------------------------------------------------
parseMap :: Parser MessageField
parseMap = do
  spaces'
  _ <- string "map"
  spaces'
  _ <- char '<'
  spaces'
  key <-
    IntKey <$> parseIntType
      <|> StringKey <$> protoName
  spaces'
  _ <- char ','
  value <- MapName <$> protoName
  spaces'
  _ <- char '>'
  spaces'
  name <- protoName
  spaces'
  _ <- char '='
  spaces'
  fieldNumber <- protoNumber
  return (MessageField (Map key value) name fieldNumber False)

-- TODO: one of

----------------------------------------------------------------

messageReserved :: Parser MessageField
messageReserved = do
  spaces'
  try parseReservedNames <|> try parseReservedNumbers

parseReservedNames :: Parser MessageField
parseReservedNames = do
  names <- reservedNames
  return (MessageReserved (ReservedMessageNames names))

parseReservedNumbers :: Parser MessageField
parseReservedNumbers = do
  numbers <- try (reservedNumbers protoNumber fieldNumberRange) `sepEndBy1` char ','
  return (MessageReserved (ReservedMessageNumbers (concat numbers)))

fieldNumberRange :: Parser FieldNumber
fieldNumberRange = do
  n <- protoNumber <|> try (string "min" >> return 1) <|> try (string "max" >> return 0xFFFFFFFF)
  return n
