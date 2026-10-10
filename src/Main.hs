{-# LANGUAGE BangPatterns #-}

import qualified Reader
import qualified Expression
import qualified Parser
import qualified Data.List as List
import qualified Data.Text as Text
import qualified TestSolve as Solu
import System.IO
import Options.Applicative
import Control.Monad (join)
import qualified Data.Text.IO as TIO
import Control.DeepSeq (NFData, force)
import qualified Solve
import qualified Data.Map as Map

data Session = Session {
    programPath :: String,
    models :: Int,
    parameters :: [(String, Int)],
    relations :: [String],
    store :: Bool
}

statements :: String -> [Expression.Statement]
statements text = case Reader.read (Parser.tokenize $ Text.pack text) of
    Right sts -> sts
    Left err    -> error err

readRelations :: String -> [String]
readRelations text = map Text.unpack $ Text.splitOn comma $ Text.pack text
    where
        comma = Text.pack ","

readParameters :: String -> [(String, Int)]
readParameters params = map splitTuple $ map (Text.splitOn colon)  $ Text.splitOn comma $ Text.pack params
    where
        colon = Text.pack ":"
        comma = Text.pack ","

splitTuple :: [Text.Text] -> (String, Int)
splitTuple [parameter, value] = (Text.unpack parameter, read $ Text.unpack value)
splitTuple arg = error ("Expected a list of the form [parameter, value], received instead: " ++ (concat $ map Text.unpack $ arg))

session :: Parser Session
session = Session
      <$> strArgument
          ( metavar "PROGRAM"
          <> help "Input program path. The file in that path must contain a sequence of well-formed statements." )
      <*> option auto
          ( long "models"
          <> short 'm'
          <> help "How many models to show."
          <> showDefault
          <> value 1
          <> metavar "MODELS" )
      <*> (readParameters <$>
                strOption
                    ( long "parameters"
                    <> short 'p'
                    <> help "Module parameters, in the format <param> : <int> (like 'n:100,m:100,w:20,k:12', etc)."
                    <> showDefault
                    <> value ""
                    <> metavar "PARAMS" ))
      <*> (readRelations <$>
                strOption
                    ( long "relations"
                    <> short 'r'
                    <> help "Names of relations to be included in the output models (only included relations will be shown in the output). For instance, in a file that specifies graphs with edges and colors using some other auxiliary predicates, you can 'see' only edges and colors by passing -r edges,colors"
                    <> showDefault
                    <> value ""
                    <> metavar "RELS" ))
      <*> switch
          ( long "store"
          <> short 's'
          <> help "Whether to store the output models to a file." )

sessionPath :: Session -> String
sessionPath (Session program models parameters relations store) = program

sessionModels :: Session -> Int
sessionModels (Session program models parameters relations store) = models

userInput :: IO Session
userInput = execParser opts
    where
        opts = info (session <**> helper)
                    ( fullDesc
                    <> progDesc "Find finite models for syntactically restricted first order theories, similar to an answer set programming engine or logic programming language."
                    <> header "Tale.hs" )

main = do
    sessionData <- userInput
    surcho <- readFile (sessionPath sessionData)
    --putStrLn $ stateRepresentation surcho ++ "\n\n" ++ theory surcho
    putStrLn $ theory surcho
        where 
            --stateRepresentation :: String -> String
            --stateRepresentation s = show $ Expression.programState $ map Solu.parse $ map Text.pack $ lines $ s
            theory :: String -> String
            theory s = List.intercalate "\n" $ map show $ Expression.getGamma $ map Solu.parse $ map Text.pack $ lines $ s