module Text.Protobuf.Parser.LexicalElement.IntegerLiteral
  ( module Text.Protobuf.Parser.LexicalElement.IntegerLiteral,
  )
where

import Text.Parsec hiding (hexDigit)
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Digit
import Text.Protobuf.Types

-- intLit     = decimalLit | octalLit | hexLit
intLit :: Parser IntLit
intLit = do
  d <- decimalLit <|> octalLit <|> hexLit
  return d

-- decimalLit = ( "1" ... "9" ) { decimalDigit }
decimalLit :: Parser DecimalLit
decimalLit = do
  d <- oneOf ['1' .. '9']
  ds <- many decimalDigit
  return $ read (d : ds)

-- octalLit   = "0" { octalDigit }
octalLit :: Parser OctalLit
octalLit = do
  char '0'
  ds <- many octalDigit
  return $ read ('0' : ds)

-- hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
hexLit :: Parser HexLit
hexLit = do
  char '0'
  oneOf "xX"
  ds <- many hexDigit
  return $ read ('0' : 'x' : ds)
