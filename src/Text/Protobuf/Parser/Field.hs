module Text.Protobuf.Parser.Field (type', fieldNumber) where

import Text.Parsec hiding (option)
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Boolean
import Text.Protobuf.Parser.LexicalElement.Constant
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.IntegerLiteral
import Text.Protobuf.Parser.LexicalElement.Space (spaces', spaces1)
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

-- type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
-- \| "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
-- \| "bool" | "string" | "bytes" | messageType | enumType
parseDataType :: Parser FieldType
parseDataType =
  Scalar
    <$> parseScalarType
      <|> Compound
    <$> protoName

type' :: Parser FieldType
type' =
  Scalar
    <$> parseScalarType
      <|> Compound
    <$> protoName

-- fieldNumber = intLit;
fieldNumber :: Parser Int
fieldNumber = intLit
