module ProtoParser.Message (parseMessage, parseMessage') where

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
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [], enums = [], messages = [x], services = []})
    )

parseMessage :: Parser Message
parseMessage = parseMessage''

parseMessage'' :: Parser Message
parseMessage'' = do
  spaces'
  _ <- string "message"
  spaces1
  name <- protoName
  spaces'
  _ <- char '{'
  spaces'
  fields <- try parseMessageField `sepEndBy` char ';'
  spaces'
  _ <- char '}'
  spaces'
  return (Message name fields)

parseDataType :: Parser DataType
parseDataType =
  do
    Scalar <$> parseScalarType
    <|> Compound <$> protoName

parseMessageField :: Parser MessageField
parseMessageField =
  do
    try implicitMessageField
    <|> try optionalMessageField
    <|> try repeatedMessageField
    <|> try reservedMessageField

implicitMessageField :: Parser MessageField
implicitMessageField = do
  spaces'
  t <- parseDataType <|> parseMap
  spaces'
  name <- protoName
  spaces'
  _ <- char '='
  spaces'
  fieldNumber <- protoNumber
  spaces'
  return (ImplicitMessageField t name fieldNumber)

optionalMessageField :: Parser MessageField
optionalMessageField = do
  spaces'
  _ <- string "optional"
  spaces'
  t <- parseDataType <|> parseMap
  spaces'
  name <- protoName
  spaces'
  _ <- char '='
  spaces'
  fieldNumber <- protoNumber
  spaces'
  return (OptionalMessageField t name fieldNumber)

repeatedMessageField :: Parser MessageField
repeatedMessageField = do
  spaces'
  _ <- string "repeated"
  spaces'
  t <- parseDataType
  spaces'
  name <- protoName
  spaces'
  _ <- char '='
  spaces'
  fieldNumber <- protoNumber
  spaces'
  return (RepeatedMessageField t name fieldNumber)

reservedMessageField :: Parser MessageField
reservedMessageField = do
  spaces'
  _ <- string "reserved"
  spaces'
  try parseReservedNames <|> try parseReservedNumbers

----------------------------------------------------------------

-- TODO: one of

----------------------------------------------------------------

parseReservedNames :: Parser MessageField
parseReservedNames = do
  MessageReserved . ReservedMessageNames <$> reservedNames

parseReservedNumbers :: Parser MessageField
parseReservedNumbers = do
  numbers <- try (reservedNumbers protoNumber fieldNumberRange) `sepEndBy1` char ','
  return (MessageReserved (ReservedMessageNumbers (concat numbers)))

fieldNumberRange :: Parser FieldNumber
fieldNumberRange = do
  protoNumber <|> try (string "min" >> return 1) <|> try (string "max" >> return 0xFFFFFFFF)
