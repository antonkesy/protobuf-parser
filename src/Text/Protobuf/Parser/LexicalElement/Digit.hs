module Text.Protobuf.Parser.LexicalElement.Digit
  ( module Text.Protobuf.Parser.LexicalElement.Digit,
  )
where

import Text.Parsec
import Text.Parsec.String

-- decimalDigit = "0" ... "9"
decimalDigit :: Parser Char
decimalDigit = oneOf ['0' .. '9']

-- octalDigit   = "0" ... "7"
octalDigit :: Parser Char
octalDigit = oneOf ['0' .. '7']

-- hexDigit     = "0" ... "9" | "A" ... "F" | "a" ... "f"
hexDigit :: Parser Char
hexDigit = oneOf (['0' .. '9'] ++ ['A' .. 'F'] ++ ['a' .. 'f'])
