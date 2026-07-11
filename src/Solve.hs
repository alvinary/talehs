{-# LANGUAGE ScopedTypeVariables #-}

module Solve where

import SAT.MiniSat
import qualified Expression
import qualified Data.Map as Map

getLiteral :: Expression.Conjunction ->Expression.Literal
getLiteral (Expression.Mono l) = l
getLiteral _ = error "Cannot extract literals from non-mono conjunct"

toLiterals :: Expression.Formula -> [([Expression.Literal], [Expression.Literal])]
toLiterals (Expression.Contradiction cs) = [(map getLiteral cs, [])]
toLiterals (Expression.Disjunction cs) = [([], map getLiteral cs)]
toLiterals (Expression.Assertion cs) = concat $ map (\x -> toLiterals (Expression.Implication [] [x])) cs
toLiterals (Expression.Equivalence cs ks) = toLiterals (Expression.Implication cs ks) ++ toLiterals (Expression.Implication ks cs)
toLiterals (Expression.Implication hs cs) = map clausify $ map getLiteral cs
    where
        clausify literal = (hypo, [literal])
        hypo = map getLiteral hs

asMinisat :: ([Expression.Literal], [Expression.Literal]) -> Formula Expression.Literal
asMinisat (negatives, positives) = Some (allNegatives ++ allPositives)
    where
        allNegatives = map toNegative negatives
        allPositives = map toPositive positives
        toPositive x = Var x
        toNegative x = Not $ Var x

getModels :: [Expression.Statement] -> Int -> [Map.Map Expression.Literal Bool]
getModels program n = take n $ solve_all $ All formulas
    where
        formulas :: [Formula Expression.Literal] = map asMinisat literals
        literals :: [([Expression.Literal], [Expression.Literal])] = concat $ map toLiterals gamma
        gamma :: [Expression.Formula] = Expression.getGamma program