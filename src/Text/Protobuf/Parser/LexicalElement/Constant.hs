module Text.Protobuf.Parser.LexicalElement.Constant
  ( module Text.Protobuf.Parser.LexicalElement.Constant,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Boolean
import Text.Protobuf.Parser.LexicalElement.FloatingPointLiteral
import Text.Protobuf.Parser.LexicalElement.Identifier
import Text.Protobuf.Parser.LexicalElement.IntegerLiteral
import Text.Protobuf.Parser.LexicalElement.StringLiteral
import Text.Protobuf.Types

-- constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit | MessageValue
constant :: Parser Constant
constant =
  (ConstantBoolLit <$> try boolLit)
    <|> (ConstantFloatLit <$> try floatLit)
    <|> (ConstantIntLit <$> try intLit)
    <|> (ConstantStrLit <$> try strLit)
    <|> (ConstantFullIdent <$> try fullIdent)
    -- <|> try (ConstantMessageValue <$> messageValue)
    <?> "Expected constant"

-- MessageValue = "{", Message, "}" | "<", Message, ">" ;
-- messageValue :: Parser MessageValue
-- messageValue =
--   try ((char '{' *> message <* char '}'))
--     <|> try ((char '<' *> message <* char '>'))
--     <?> "Expected message value"
