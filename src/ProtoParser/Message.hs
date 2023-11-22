module ProtoParser.Message (parseMessage, parseMessage') where

import Data.Maybe (catMaybes)
import ProtoParser.Misc
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseMessage' :: Parser Protobuf
parseMessage' = do
  x <- parseMessage
  return (Protobuf {package = [], imports = [], options = [], enums = [], messages = [x], services = []})

parseMessage :: Parser Message
parseMessage = do
  parseMessage''

parseMessage'' :: Parser Message
parseMessage'' = do
  spaces
  _ <- string "message"
  spaces1
  name <- protoName
  spaces
  _ <- char '{'
  spaces
  -- TODO: multiple inputs
  fields <- parseMessageField `sepEndBy1` char ';'
  spaces
  _ <- char '}'
  return (Message name (catMaybes fields))

parseMessageField :: Parser (Maybe MessageField)
parseMessageField = do
  return Nothing

-- return (Just (MessageField t "" 0 False))
-- t :: ProtoDataType
-- t = return MessageName ""
