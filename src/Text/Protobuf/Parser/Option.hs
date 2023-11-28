module Text.Protobuf.Parser.Option (parseOption, parseOption', parseFieldOption) where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Space (spaces', spaces1)
import Text.Protobuf.Parser.Type (parseBool, parseCustomName, parseString, protoName)
import Text.Protobuf.Types

parseOption' :: Protobuf -> Parser Protobuf
parseOption' p = do
  opt <- parseOption
  return
    ( Text.Protobuf.Types.merge
        p
        ( Protobuf
            { syntax = Nothing,
              package = Nothing,
              imports = [],
              options = [opt],
              enums = [],
              messages = [],
              services = []
            }
        )
    )

parseOption :: Parser Option
parseOption =
  Option
    <$> ( spaces'
            *> string "option"
            *> spaces1
            *> protoName
            <* spaces1
        )
    <*> ( spaces'
            *> char '='
            *> spaces'
            *> ( (StringValue <$> try parseString)
                   <|> (BoolValue <$> try parseBool)
                   <|> (CompoundValue <$> try protoName)
               )
            <* spaces'
            <* char ';'
        )

parseFieldOption :: Parser [FieldOption]
parseFieldOption =
  start
    *> try ((try singleFieldOption `sepBy1` try (char ',')) <* end)
  where
    start = spaces' *> char '[' *> spaces'
    end = spaces' <* char ']'
    name = spaces' *> (try protoName <|> parseCustomName) <* spaces'
    value =
      spaces'
        *> char '='
        *> spaces'
        *> ( (StringValue <$> try parseString)
               <|> (BoolValue <$> try parseBool)
               <|> (CompoundValue <$> try protoName)
           )
        <* spaces'
    singleFieldOption =
      FieldOption
        <$> name
        <*> value
