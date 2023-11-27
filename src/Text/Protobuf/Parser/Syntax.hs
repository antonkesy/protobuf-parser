module Text.Protobuf.Parser.Syntax (parseSyntax, parseSyntax') where

import qualified Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Space (spaces')
import Text.Protobuf.Types

parseSyntax' :: Protobuf -> Parser Protobuf
parseSyntax' p = do
  syn <- parseSyntax
  if Data.Maybe.isJust (syntax p)
    then unexpected ": There can only be one syntax definition per file"
    else
      return
        ( Text.Protobuf.Types.merge
            p
            (Protobuf {syntax = Just syn, package = Nothing, imports = [], options = [], enums = [], messages = [], services = []})
        )

parseSyntax :: Parser Syntax
parseSyntax =
  spaces'
    *> string "syntax"
    *> spaces'
    *> char '='
    *> spaces'
    *> char '"'
    *> ( (try (string "proto2") >> return Proto2)
           <|> (try (string "proto3") >> return Proto3)
       )
    <* char '"'
    <* spaces'
    <* char ';'
