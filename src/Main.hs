import qualified Reader
import qualified Expression
import qualified Parser
import qualified Data.Text as Text
import System.IO
import Options.Applicative
import Control.Monad (join)

data Session = Session {
    programPath :: String,
    models :: Int,
    parameters :: String,
    relations :: String,
    store :: Bool
}

statements :: String -> [Expression.Statement]
statements text = Reader.read $ Parser.tokenize $ Text.pack text

{-
main :: IO ()
main = do
    contents <- readFile path    
    let fileLines = lines contents
    mapM_ putStrLn fileLines
-}

session :: Parser Session
session = Session
      <$> strOption
          ( long "input"
          <> metavar "PROGRAM"
          <> help "Input program path. The file in that path must contain a sequence of well-formed statements." )
      <*> option auto
          ( long "models"
          <> short 'm'
          <> help "How many models to show."
          <> showDefault
          <> value 1
          <> metavar "MODELS" )
      <*> strOption
          ( long "parameters"
          <> short 'p'
          <> help "Module parameters, in the format <param> : <int> (like 'n:100,m:100,w:20,k:12', etc)."
          <> showDefault
          <> value ""
          <> metavar "PARAMS" )
      <*> strOption
          ( long "relations"
          <> short 'r'
          <> help "Names of relations to be included in the output models (only included relations will be shown in the output). For instance, in a file that specifies graphs with edges and colors using some other auxiliary predicates, you can 'see' only edges and colors by passing -r edges,colors"
          <> showDefault
          <> value ""
          <> metavar "RELS" )
      <*> switch
          ( long "store"
          <> short 's'
          <> help "Whether to store the output models to a file." )

run :: Session -> IO ()
run (Session program models paramters relations store) = putStrLn program
run _ = return ()

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (session <**> helper)
      ( fullDesc
     <> progDesc "Find finite models for syntactically restricted first order theories, similar to an answer set programming engine or logic programming language."
     <> header "Tale.hs" )

