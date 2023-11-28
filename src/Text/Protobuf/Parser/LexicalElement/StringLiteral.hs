module Text.Protobuf.Parser.LexicalElement.StringLiteral
  ( module Text.Protobuf.Parser.LexicalElement.StringLiteral,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Types

-- strLit = strLitSingle { strLitSingle }
strLit :: Parser StringLiteral
strLit = do
  s <- strLitSingle
  ss <- many strLitSingle
  return (s ++ concat ss)

-- strLitSingle = ( "'" { charValue } "'" ) |  ( '"' { charValue } '"' )
strLitSingle :: Parser StringLiteralSingle
strLitSingle = do
  c <- oneOf "'\""
  cs <- many anyChar
  c' <- oneOf "'\""
  return ((c : cs) ++ [c'])

-- TODO:
-- charValue = hexEscape | octEscape | charEscape | unicodeEscape | unicodeLongEscape | /[^\0\n\\]/
-- hexEscape = '\' ( "x" | "X" ) hexDigit [ hexDigit ]
-- octEscape = '\' octalDigit [ octalDigit [ octalDigit ] ]
-- charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
-- unicodeEscape = '\' "u" hexDigit hexDigit hexDigit hexDigit
-- unicodeLongEscape = '\' "U" ( "000" hexDigit hexDigit hexDigit hexDigit hexDigit |
--                               "0010" hexDigit hexDigit hexDigit hexDigit
