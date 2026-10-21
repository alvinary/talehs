{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSolve where

import qualified Solve
import Expression
import Reader
import Parser
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

parse :: Text.Text -> Statement
parse text =
  case Reader.read tokens of
    Right (x:[]) -> x
    Right (x:xs) -> error "Unexpected return type while parsing"
    Left (rest, expected) -> errorText (rest, expected, take (length tokens - length rest) tokens, tokens)
  where
    tokens = Parser.tokenize text

errorText (t : ts, expected, remaining, tokens) = error ("unexpected " ++ show t ++ ";\nexpected one of: " ++ unwords expected ++ ";\ntokens remaining in current line: \n" ++ (unwords $ map showToken remaining) ++ "\n In line: " ++  (unwords $ map showToken tokens))
errorText ([], expected, remaining, tokens)     = error ("unexpected end of input; expected one of: " ++ unwords expected ++ ";\nRan out of input while reading " ++ (unwords $ map showToken tokens))


extract :: Statement -> Declaration
extract (Dec d) = d
extract _ = error "Horror"

parseDeclaration :: Text.Text -> Declaration
parseDeclaration text = (\(Dec x) -> x) $ parse text

parseRule :: Text.Text -> Formula
parseRule text = (\(For x) -> x) $ parse text

checkOrder orderDeclaration n sortName = cardinalIsOk && hasFirst && hasLast && hasNext
    where
        cardinalIsOk = (length $ sortMembers) == n
        hasFirst = True
        hasLast = True
        hasNext = (Map.findWithDefault (Leaf "") (head sortMembers, Leaf "next") $ values state) == (head $ tail $ sortMembers)
        sortMembers = Map.findWithDefault [] (Leaf sortName) $ members state
        state :: State = Expression.stateUpdate declaration Expression.emptyState
        declaration = (Expression.Order (Leaf "i") (Leaf "25") (Leaf "A"))

orderCase = "order i 25 : A"
orderCaseB = "order p 15 : A"

lolo = checkOrder orderCase 25 "A"

abcConstants = "order a 600: A"
colors = "const red, blue : Color"
functionG = "let g : A -> Color"
xyVars = "var x, y, z : A"
colorVars = "var k : Color"
previous = "let x.next.previous = x"

checkFunctions functionDeclaration functionName state = allInDomain && boundsForbidden
    where
        allInDomain = False
        boundsForbidden = False 

stato = getState $ map parseDeclaration simpleDeclarations
funs = Expression.functions $ getState $ map parseDeclaration simpleDeclarations

simpleDeclarations = [abcConstants, xyVars]
instanceDeclarations = [abcConstants, colors, functionG, xyVars, colorVars]
instanceRules = map Text.pack ["r(x, y) -> not r (y, x)", "r (x, x) -> False", "r(x, y), c(x, k), c(y, k) -> False"]
simpleRules = map Text.pack ["r(x, y), r(y, z) -> r(x, z)", "r(x, y), r(y, x) -> False"] -- "m(x) -> w : A | { p ( x , w ) }", "m (a[1].next.next)"]
simpleFormulas = (map parseRule simpleRules) -- ++ [(Expression.Assertion [(Expression.Mono (Expression.Positive (Expression.Relation (Expression.Leaf "m") [(Expression.Attribute (Expression.Attribute (Expression.Index (Leaf "a") [(Leaf "1")]) (Expression.Leaf "next")) (Expression.Leaf "next"))])))])]

sampleState = Expression.getState $ map parseDeclaration simpleDeclarations
instanceState = Expression.getState $ map parseDeclaration simpleDeclarations

wowo = Expression.unfoldInstance sampleState simpleFormulas

conclusion (Implication h c) = c
conclusion x = []

lelor = simpleFormulas !! 2
lolir = conclusion lelor
welor = map (bindPoly (lolir !! 0) (members sampleState) ) $ (getAssignments lelor sampleState)
walor = map (bindAny (lolir !! 0) (members sampleState) ) $ (getAssignments lelor sampleState)
wulor = map (bindAny lelor (members sampleState) ) $ (getAssignments lelor sampleState)

lolodor = (wobtain leForm, Expression.bindAny (gobtain leForm) (Expression.members sampleState) $ Map.fromList [("x", (Expression.Leaf "a"))])
    where
        leForm = simpleFormulas !! 2
        wobtain (Implication a b) = obtain $ head b
        wobtain _ = error "Was expecting an implication"
        gobtain (Implication a b) = head b
        obtain (Poly head body) = Set.toList $ bigUnion $ map (\(x, y) -> leaves x) head
        obtain _ = []

wukong = Expression.getGamma simpleProgram

woochi :: IO ()
woochi = Expression.showInLines $ map showPositive $ Solve.getModels simpleProgram 1
    where
        showPositive m = filter (\x -> not(Text.isInfixOf "-" $ Expression.tshow x)) $ map (\(x, y) -> x) $ filter (\(x, y) -> y == True) $ Map.toList m

ciaccona :: Int
ciaccona = length $ map showPositive $ Solve.getModels simpleProgram 1
    where
        showPositive m = filter (\x -> not(Text.isInfixOf "¬" $ Expression.tshow x)) $ map (\(x, y) -> x) $ filter (\(x, y) -> y == True) $ Map.toList m

instanceProgram = map fromDeclaration instanceDeclarations ++ map fromFormula instanceRules
    where
        fromDeclaration d = Dec $ parseDeclaration d
        fromFormula f = For $ parseRule f

simpleProgram = map fromDeclaration instanceDeclarations ++ map Expression.For simpleFormulas
    where
        fromDeclaration d = Dec $ parseDeclaration d
        fromFormula f = For $ parseRule f

taoqi = Expression.getGamma instanceProgram

tyrfing = Expression.getGamma simpleProgram

calcharo :: Int -> IO ()
calcharo k = Expression.showInLines $ map showPositive $ Solve.getModels instanceProgram k
    where
        showPositive m = filter (\x -> not(Text.isInfixOf "¬" $ Expression.tshow x)) $ map (\(x, y) -> x) $ filter (\(x, y) -> y == True) $ Map.toList m


