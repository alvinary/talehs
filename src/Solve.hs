import MiniSat
import qualified Expression

toMiniSat :: Expression.Formula -> Formula Expression.Literal
toMiniSat f = All []

requireAll [] = Yes
requireAll f:fs = f :&&: fs

getModels :: [Expression.Statement] -> Int -> [Map Literal Bool]
getModels program n = solve_all $ requireAll $ map toMiniSat [Expression.getGamma program]