module Text.Protobuf.Parser.LexicalElement.FloatingPointLiteral
  ( module Text.Protobuf.Parser.LexicalElement.FloatingPointLiteral,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Digit
import Text.Protobuf.Parser.Space
import Text.Protobuf.Types
import Prelude hiding (exponent)

-- floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
floatLit :: Parser FloatLit
floatLit = do
  d <- decimals
  char '.'
  d' <- decimals
  e <- optionMaybe exponent
  -- TODO: missing and incorrect
  return (-1)

-- decimals  = decimalDigit { decimalDigit }
decimals :: Parser String
decimals = many1 decimalDigit

-- exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
exponent :: Parser Int
exponent = do
  oneOf "eE"
  sign <- optionMaybe $ oneOf "+-"
  d <- decimals
  return $ read $ maybe "" (: []) sign ++ d
