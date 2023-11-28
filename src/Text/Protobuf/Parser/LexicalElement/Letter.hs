module Text.Protobuf.Parser.LexicalElement.Letter
  ( module Text.Protobuf.Parser.LexicalElement.Letter,
  )
where

import Text.Parsec
import Text.Parsec.String

-- letter = "A" ... "Z" | "a" ... "z"
letter :: Parser Char
letter = oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z']
