module ProtoParser.Misc
  ( whitespace,
    protoName,
    protoNumber,
    spaces1,
    eol,
  )
where

import Control.Monad (void)
import Protobuf
import Text.Parsec
import Text.Parsec.String

whitespace :: Parser ()
whitespace = void (many (oneOf " \n\t")) <?> "whitespace"

----------------------------------------------------------------

spaces1 :: Parser ()
spaces1 = skipMany1 space

----------------------------------------------------------------

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
        if n >= 19000 && n <= 19999
          then fail "number reserved"
          else
            if n >= 1 && n <= 536870911 -- Range from 1 to 536,870,911
              then return n
              else fail "number out of range"

----------------------------------------------------------------

-- TODO : test
eol :: Parser ()
eol = void (char '\n') <|> eof

----------------------------------------------------------------

-- TODO: maps
