module Text.Protobuf.Parser.LexicalElement.Boolean
  ( boolLit,
  )
where

import Text.Parsec
import Text.Parsec.String

-- boolLit = "true" | "false"
boolLit :: Parser Bool
boolLit =
  try (string "true" >> return True)
    <|> (string "false" >> return False)
    <?> "Expected true or false"
