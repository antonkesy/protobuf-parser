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

parseMessageField :: Parser MessageField
parseMessageField = do
  spaces'
  -- _ <- try (string "repeated") -- TODO: optional
  -- spaces'
  -- TODO: extra function parser for reserved to avoid this workaround
  t <- parseDataType
  case t of
    (Compound "reserved") -> messageReserved
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

-- TODO: one of

----------------------------------------------------------------

messageReserved :: Parser MessageField
messageReserved = do
  spaces'
  try parseReservedNames <|> try parseReservedNumbers

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
