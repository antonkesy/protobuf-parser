module ProtoParser.Reserved
  ( module ProtoParser.Reserved,
  )
where

import ProtoParser.Space (spaces')
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

reservedNames :: Parser ReservedNames
reservedNames =
  ReservedNames
    <$> try (spaces' *> char '\"' *> protoName <* char '\"') `sepBy1` char ','

reservedNumbers :: (Integral a) => Parser a -> Parser a -> Parser [a]
reservedNumbers single range =
  concat <$> try (numbers `sepBy1` char ',')
  where
    numbers =
      try
        ( (\l r -> [l .. r])
            <$> range
            <* spaces'
            <* string "to"
            <* spaces'
            <*> range
        )
        <|> ((: []) <$> try (spaces' *> single <* spaces'))
