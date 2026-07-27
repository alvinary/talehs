import qualified Reader
import qualified Expression
import qualified Parser
import qualified Data.Text as Text
import System.IO
import Options.Applicative
import Control.Monad (join)

getProgram :: String -> [Expression.Statement]
getProgram line = Reader.read $ Parser.tokenize $ Text.pack line

{-
main :: IO ()
main = do
    contents <- readFile path    
    let fileLines = lines contents
    mapM_ putStrLn fileLines
-}


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "General program title/description"
  <> progDesc "What does this thing do?"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> strOption
            (  long "string_param"
            <> short 's'
            <> metavar "STRING"
            <> help "string parameter"
            )
        <*> option auto
            (  long "number_param"
            <> short 'n'
            <> metavar "NUMBER"
            <> help "number parameter"
            <> value 1
            <> showDefault
            )

work :: String -> Int -> IO ()
work _ _ = return ()