module Text.Protobuf.Parser.Reserved
  ( module Text.Protobuf.Parser.Reserved,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Space (spaces')
import Text.Protobuf.Parser.Type
import Text.Protobuf.Types

reservedNames :: Parser ReservedNames
reservedNames =
  ReservedNames
    <$> try
      ( spaces'
          *> char '\"'
          *> protoName
          <* char '\"'
      )
      `sepBy1` char ','

reservedNumbers :: (Integral a) => Parser a -> Parser a -> Parser [a]
reservedNumbers single range =
  concat <$> try (numbers `sepBy1` char ',')
  where
    numbers =
      try
        ( (\l r -> [l .. r])
            <$> (spaces' *> range)
            <* spaces'
            <* string "to"
            <* spaces'
            <*> range
        )
        <|> ((: []) <$> try (spaces' *> single <* spaces'))
