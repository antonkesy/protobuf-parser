module Text.Protobuf.Parser.Message (parseMessage, parseMessage') where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Enum
import Text.Protobuf.Parser.Option
import Text.Protobuf.Parser.Reserved
import Text.Protobuf.Parser.Space (spaces', spaces1)
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

parseMessage' :: Protobuf -> Parser Protobuf
parseMessage' p = do
  x <- parseMessage
  return
    ( Text.Protobuf.Types.merge
        p
        ( Protobuf
            { syntax = Nothing,
              package = Nothing,
              imports = [],
              options = [],
              enums = [],
              messages = [x],
              services = []
            }
        )
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
            *> many parseMessageField
            <* spaces'
            <* char '}'
            <* spaces'
        )

parseMessageField :: Parser MessageField
parseMessageField =
  spaces'
    *> ( try implicitField <* fieldSeparator
           <|> try (optionalField <* fieldSeparator)
           <|> try (repeatedField <* fieldSeparator)
           <|> try (reservedField <* fieldSeparator)
           <|> try oneofField
           <|> (OptionMessageField <$> try parseOption <* fieldSeparator)
           <|> (EnumMessageField <$> try parseEnum)
           <|> (NestedMessage <$> try parseMessage'')
       )
  where
    fieldName = spaces' *> protoName
    fieldNumber = spaces' *> char '=' *> spaces' *> protoNumber
    fields = try (many parseMessageField)
    fieldOptions = try parseFieldOption <|> return []
    fieldSeparator = spaces' *> char ';' *> spaces'

    reservedValues =
      try (ReservedMessageNames <$> reservedNames)
        <|> try
          ( ReservedMessageNumbers
              <$> reservedNumbers protoNumber fieldNumberRange
          )
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
fieldNumberRange =
  do
    protoNumber
    <|> try (string "min" >> return 1)
    <|> try (string "max" >> return 0xFFFFFFFF)
