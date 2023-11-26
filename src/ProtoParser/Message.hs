module ProtoParser.Message (parseMessage, parseMessage') where

import ProtoParser.Option
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
parseMessage'' =
  Message
    <$> ( spaces'
            *> string "message"
            *> spaces1
            *> protoName
        )
    <*> ( spaces'
            *> char '{'
            *> spaces'
            *> fields
            <* spaces'
            <* char '}'
            <* spaces'
        )
  where
    fields = try parseMessageField `sepEndBy` char ';'

parseMessageField :: Parser MessageField
parseMessageField =
  spaces'
    *> ( try implicitField
           <|> try optionalField
           <|> try repeatedField
           <|> try reservedField
           <|> try oneofField
       )
  where
    fieldName = spaces' *> protoName
    fieldNumber = spaces' *> char '=' *> spaces' *> protoNumber
    fields = try parseMessageField `sepEndBy` char ';'
    fieldOptions = try parseFieldOption <|> return []

    reservedValues =
      try (ReservedMessageNames <$> reservedNames)
        <|> try (ReservedMessageNumbers <$> reservedNumbers protoNumber fieldNumberRange)
    implicitField =
      ImplicitMessageField
        <$> (try parseDataType <|> try parseMap)
        <*> fieldName
        <*> fieldNumber
        <*> fieldOptions
    optionalField =
      OptionalMessageField
        <$> ( string "optional"
                *> spaces'
                *> (try parseDataType <|> try parseMap)
            )
        <*> fieldName
        <*> fieldNumber
        <*> fieldOptions
    repeatedField =
      RepeatedMessageField
        <$> ( string "repeated"
                *> spaces'
                *> parseDataType -- maps not allowed in repeated fields
            )
        <*> fieldName
        <*> fieldNumber
        <*> fieldOptions
    reservedField =
      MessageReserved
        <$> ( string "reserved"
                *> spaces'
                *> reservedValues
            )
    oneofField =
      OneOfMessageField
        <$> ( string "oneof"
                *> spaces'
                *> protoName
            )
        <*> ( spaces'
                *> char '{'
                *> spaces'
                *> fields
                <* spaces'
                <* char '}'
                <* spaces'
            )

fieldNumberRange :: Parser FieldNumber
fieldNumberRange = do
  protoNumber <|> try (string "min" >> return 1) <|> try (string "max" >> return 0xFFFFFFFF)
