module Main (main) where

import Options.Applicative
import Prettyprinter
  ( Pretty (pretty),
    defaultLayoutOptions,
    layoutPretty,
  )
import Prettyprinter.Render.String (renderString)
import Text.Protobuf.Parser
import Text.Protobuf.Types

data Options = Options (Maybe FilePath) Bool [String]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> optional
      ( strOption
          (long "file" <> short 'f' <> metavar "PATH" <> help "Specify file path to parse")
      )
    <*> switch (long "pretty" <> short 'p' <> help "Enable pretty print")
    <*> many (argument str (metavar "STRING..."))

main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper) fullDesc
  processOptions opts

processOptions :: Options -> IO ()
processOptions (Options Nothing False []) =
  putStrLn "Arguments: No file path provided or strings provided"
processOptions (Options (Just path) isPrettier []) = do
  result <- parseProtoFile path
  case result of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right protobuf ->
      protoPrint protobuf isPrettier
processOptions (Options _ isPrettier otherArgs) =
  case parseProtobuf (unwords otherArgs) of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right protobuf ->
      protoPrint protobuf isPrettier

protoPrint :: Protobuf -> Bool -> IO ()
protoPrint protobuf isPrettier =
  if not isPrettier
    then putStrLn $ show protobuf
    else
      putStrLn $
        renderString $
          layoutPretty defaultLayoutOptions $
            pretty protobuf
