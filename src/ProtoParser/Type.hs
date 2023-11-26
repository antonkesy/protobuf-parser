module ProtoParser.Type
  ( module ProtoParser.Type,
  )
where

import ProtoParser.Space (spaces')
import Protobuf
import Text.Parsec
import Text.Parsec.String

-- TODO: rename parseName
protoName :: Parser String
protoName = do
  (:)
    <$> letter
    <*> many (alphaNum <|> char '_')

-- TODO: rename parseFieldNumber
protoNumber :: Parser FieldNumber
protoNumber = do
  n <- read <$> many1 digit
  case () of
    -- 19,000 to 19,999 are reserved for the Protocol Buffers
    _ | n >= 19000 && n < 20000 -> fail "number reserved"
    -- FieldNumber  [1..536,870,911]
    _ | n <= 0 || n > 536870911 -> fail "number out of range"
    _ -> return n

parseIntType :: Parser IntType
parseIntType =
  (string "int32" >> return Int32)
    <|> (string "int64" >> return Int64)
    <|> (string "uint32" >> return UInt32)
    <|> (string "uint64" >> return UInt64)
    <|> (string "sint32" >> return SInt32)
    <|> (string "sint64" >> return SInt64)
    <|> (string "fixed32" >> return Fixed32)
    <|> (string "fixed64" >> return Fixed64)
    <|> (string "sfixed32" >> return SFixed32)
    <|> (string "sfixed64" >> return SFixed64)

parseStringType :: Parser MapKey
parseStringType = StringKey <$> protoName

parseScalarType :: Parser ScalarType
parseScalarType =
  (IntType <$> try parseIntType)
    <|> try (string "double" >> return (FloatType Double))
    <|> try (string "float" >> return (FloatType Float))
    <|> try (string "string" >> return StringType)
    <|> try (string "bytes" >> return BytesType)

parseMap :: Parser DataType
parseMap =
  Map
    <$> ( string "map"
            *> spaces'
            *> char '<'
            *> spaces'
            *> (IntKey <$> parseIntType <|> StringKey <$> protoName)
        )
    <*> ( spaces'
            *> char ','
            *> spaces'
            *> (MapName <$> protoName)
            <* spaces'
            <* char '>'
            <* spaces'
        )

parseDataType :: Parser DataType
parseDataType =
  Scalar <$> parseScalarType
    <|> Compound <$> protoName

parseBool :: Parser Bool
parseBool =
  try (string "true" >> return True)
    <|> (string "false" >> return False)
    <?> "Expected true or false"

-- TODO: replace all
parseString :: Parser String
parseString =
  char '\"'
    *> manyTill anyChar (char '\"')
    <* spaces'

parseCustomName :: Parser String
parseCustomName =
  char '('
    *> ((\x -> "(" ++ x ++ ")") <$> manyTill anyChar (char ')'))
    <* spaces'
