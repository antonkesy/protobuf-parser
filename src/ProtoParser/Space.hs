module ProtoParser.Space
  ( space',
    spaces',
    spaces1,
  )
where

import Control.Monad (void)
import ProtoParser.Comment (removeComment)
import Text.Parsec
import Text.Parsec.String

space' :: Parser ()
space' = (void space <|> removeComment)

spaces' :: Parser ()
spaces' = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'
