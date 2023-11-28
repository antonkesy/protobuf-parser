module Text.Protobuf.Parser.LexicalElement.EmptyStatement
  ( module Text.Protobuf.Parser.LexicalElement.EmptyStatement,
  )
where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Types

-- emptyStatement = ";"
emptyStatement :: Parser EmptyStatement
emptyStatement = do
  void $ char ';'
