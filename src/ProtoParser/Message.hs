module ProtoParser.Message (parseMessage, parseMessage', parseMap) where

import Data.Maybe (catMaybes)
import ProtoParser.Misc
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

----------------------------------------------------------------
parseIntType :: Parser MapKey
parseIntType =
  let int32 = string "int32" >> return (IntKey Int32)
      int64 = string "int64" >> return (IntKey Int64)
      uint32 = string "uint32" >> return (IntKey UInt32)
      uint64 = string "uint64" >> return (IntKey UInt64)
      sint32 = string "sint32" >> return (IntKey SInt32)
      sint64 = string "sint64" >> return (IntKey SInt64)
      fixed32 = string "fixed32" >> return (IntKey Fixed32)
      fixed64 = string "fixed64" >> return (IntKey Fixed64)
      sfixed32 = string "sfixed32" >> return (IntKey SFixed32)
      sfixed64 = string "sfixed64" >> return (IntKey SFixed64)
   in do
        int32
        <|> int64
        <|> uint32
        <|> uint64
        <|> sint32
        <|> sint64
        <|> fixed32
        <|> fixed64
        <|> sfixed32
        <|> sfixed64

----------------------------------------------------------------
parseStringType :: Parser MapKey
parseStringType = do
  StringKey <$> protoName

----------------------------------------------------------------
-- TODO: maps
parseMap :: Parser MessageField
parseMap = do
  spaces
  _ <- string "map"
  spaces
  _ <- char '<'
  spaces
  key <- parseMapKey
  spaces
  _ <- char ','
  value <- parseMapValue
  spaces
  _ <- char '>'
  spaces
  name <- protoName -- TODO: missing
  spaces
  _ <- char '='
  spaces
  fieldNumber <- protoNumber -- TODO: missing -> convert to MessageField
  return (MessageField (Map key value) name fieldNumber False)

parseMapKey :: Parser MapKey
parseMapKey = do
  parseIntType <|> parseStringType -- order is important!

parseMapValue :: Parser MapValue
parseMapValue = do
  -- TODO: any type except other map
  MapName <$> protoName
