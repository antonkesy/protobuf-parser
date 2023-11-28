module Text.Protobuf.Parser.Reserved
  ( module Text.Protobuf.Parser.Reserved,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.IntegerLiteral
import Text.Protobuf.Parser.LexicalElement.Space (spaces')
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

-- reserved = "reserved" ( ranges | strFieldNames ) ";"
reserved :: Parser Reserved
reserved =
  Reserved
    <$> ( spaces'
            *> string "reserved"
            *> spaces'
            *> ( Left <$> ranges <|> Right <$> strFieldNames
                   <?> "Expected ranges or strFieldNames after reserved keyword"
               )
            <* spaces'
            <* char ';'
        )

-- ranges = range { "," range }
ranges :: Parser [Range]
ranges = range `sepBy1` char ','

-- range =  intLit [ "to" ( intLit | "max" ) ]
range :: Parser Range
range =
  (\l r -> [l .. r])
    <$> intLit
    <*> ( try
            (spaces' *> string "to" *> spaces' *> intLit)
            <|> (spaces' *> (string "max" >> return 0xFFFFFFFF))
        )

-- strFieldNames = strFieldName { "," strFieldName }
strFieldNames :: Parser [FieldName]
strFieldNames = strFieldName `sepBy1` char ','

-- strFieldName = "'" fieldName "'" | '"' fieldName '"'
strFieldName :: Parser FieldName
strFieldName =
  try
    ( char '\''
        *> (fieldName)
        <* char '\''
    )
    <|> ( char '"'
            *> (fieldName)
            <* char '"'
        )

-- reservedNames :: Parser ReservedNames
-- reservedNames =
--   ReservedNames
--     <$> try (spaces' *> char '\"' *> protoName <* char '\"') `sepBy1` char ','

-- reservedNumbers :: (Integral a) => Parser a -> Parser a -> Parser [a]
-- reservedNumbers single range =
--   concat <$> try (numbers `sepBy1` char ',')
--   where
--     numbers =
--       try
--         ( (\l r -> [l .. r])
--             <$> range
--             <* spaces'
--             <* string "to"
--             <* spaces'
--             <*> range
--         )
--         <|> ((: []) <$> try (spaces' *> single <* spaces'))
