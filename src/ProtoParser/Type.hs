module ProtoParser.Type
  ( module ProtoParser.Type,
  )
where

import ProtoParser.Space (spaces')
import Protobuf
import Text.Parsec
import Text.Parsec.String

protoName :: Parser String
protoName = do
  first <- letter <?> "Expected first letter to be ...?"
  rest <- many (alphaNum <|> char '_' <?> "Expected letter, number or '_'")
  return (first : rest)

----------------------------------------------------------------

protoNumber :: Parser FieldNumber
protoNumber =
  -- https://protobuf.dev/programming-guides/proto3/#assigning
  let val = (read <$> many1 digit)
   in do
        n <- val
        -- 19,000 to 19,999 are reserved for the Protocol Buffers
        if 19000 <= n && n <= 19999
          then fail "number reserved"
          else
            if 1 <= n && n <= 536870911 -- Range from 1 to 536,870,911
              then return n
              else fail "number out of range"

----------------------------------------------------------------

parseIntType :: Parser IntType
parseIntType =
  let int32 = string "int32" >> return (Int32)
      int64 = string "int64" >> return (Int64)
      uint32 = string "uint32" >> return (UInt32)
      uint64 = string "uint64" >> return (UInt64)
      sint32 = string "sint32" >> return (SInt32)
      sint64 = string "sint64" >> return (SInt64)
      fixed32 = string "fixed32" >> return (Fixed32)
      fixed64 = string "fixed64" >> return (Fixed64)
      sfixed32 = string "sfixed32" >> return (SFixed32)
      sfixed64 = string "sfixed64" >> return (SFixed64)
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

parseScalarType :: Parser ScalarType
parseScalarType =
  do
    intType <- parseIntType
    return (IntType intType)
    <|> (string "double" >> return (FloatType Double))
    <|> (string "float" >> return (FloatType Float))
    <|> (string "string" >> return StringType)
    <|> (string "bytes" >> return BytesType)

----------------------------------------------------------------

parseMap :: Parser MessageField
parseMap = do
  spaces'
  _ <- string "map"
  spaces'
  _ <- char '<'
  spaces'
  key <-
    IntKey
      <$> parseIntType
        <|> StringKey
      <$> protoName
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
