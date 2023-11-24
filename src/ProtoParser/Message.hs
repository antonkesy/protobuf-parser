module ProtoParser.Message (parseMessage, parseMessage', parseMap) where

import Data.Maybe (catMaybes)
import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseMessage' :: Protobuf -> Parser Protobuf
parseMessage' p = do
  x <- parseMessage
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
  return (Message name (catMaybes fields))

parseMessageField :: Parser (Maybe MessageField)
parseMessageField = do
  spaces'
  -- _ <- try (string "repeated") -- TODO: optional
  -- spaces'
  t <- parseDataType
  spaces'
  name <- protoName
  spaces'
  _ <- char '='
  spaces'
  fieldNumber <- protoNumber
  spaces'
  return (Just (MessageField t name fieldNumber False))

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
