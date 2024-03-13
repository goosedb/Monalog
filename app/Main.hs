module Main where
import Options.Applicative 
import Data.Maybe (fromMaybe)
import App
import Options.Applicative.Common (runParser)

parseArgs :: Parser AppArguments
parseArgs = AppArguments 
  <$> (fromMaybe Stdin <$> optional (File <$> argument str (metavar "FILE")))

main :: IO ()
main = execParser (info (parseArgs <**> helper) fullDesc) >>= app
