module Text.Protobuf.Parser.Space
  ( space',
    spaces',
    spaces1,
  )
where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.Comment (removeComment)

space' :: Parser ()
space' = void space <|> removeComment <|> void newline <|> void tab

spaces' :: Parser ()
spaces' = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'
