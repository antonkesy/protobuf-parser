module Text.Protobuf.Parser.LexicalElement.Identifier
  ( module Text.Protobuf.Parser.LexicalElement.Identifier,
  )
where

import Text.Parsec
import Text.Parsec.String
import Text.Protobuf.Parser.LexicalElement.Digit
import Text.Protobuf.Types

-- ident = letter { letter | decimalDigit | "_" }
ident :: Parser Identifier
ident = do
  first <- letter
  rest <- many (letter <|> decimalDigit <|> char '_')
  return $ first : rest

-- fullIdent = ident { "." ident }
fullIdent :: Parser FullIdentifier
fullIdent = do
  first <- ident
  rest <- many ((:) <$> char '.' <*> ident)
  return $ first ++ concat rest

-- messageName = ident
messageName :: Parser MessageName
messageName = ident

-- enumName = ident
enumName :: Parser EnumName
enumName = ident

-- fieldName = ident
fieldName :: Parser FieldName
fieldName = ident

-- oneofName = ident
oneofName :: Parser OneofName
oneofName = ident

-- mapName = ident
mapName :: Parser MapName
mapName = ident

-- serviceName = ident
serviceName :: Parser ServiceName
serviceName = ident

-- rpcName = ident
rpcName :: Parser RpcName
rpcName = ident

-- messageType = [ "." ] { ident "." } messageName
messageType :: Parser MessageType
messageType = do
  first <- optionMaybe (char '.')
  rest <- many (ident <* char '.')
  name <- messageName
  return $ case first of
    Nothing -> name
    Just _ -> '.' : concat rest ++ name

-- enumType = [ "." ] { ident "." } enumName
enumType :: Parser EnumType
enumType = do
  first <- optionMaybe (char '.')
  rest <- many (ident <* char '.')
  name <- enumName
  return $ case first of
    Nothing -> name
    Just _ -> '.' : concat rest ++ name
