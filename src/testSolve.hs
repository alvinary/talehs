import qualified Solve
import Expression
import Reader
import Parser
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List

parse text = head $ Reader.read $ Parser.tokenize $ Text.pack $ text

extract :: Statement -> Declaration
extract (Dec d) = d
extract _ = error "Horror"

parseDeclaration :: String -> Declaration
parseDeclaration text = (\(Dec x) -> x) $ parse text

parseRule :: String -> Formula
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

abcConstants = "const a, b, c, d, e : A"
xyVars = "var x, y, z : A"

checkFunctions functionDeclaration functionName state = allInDomain && boundsForbidden
    where
        allInDomain = False
        boundsForbidden = False 

stato = getState $ map parseDeclaration simpleDeclarations
funs = Expression.functions $ getState $ map parseDeclaration simpleDeclarations

simpleDeclarations = [abcConstants, xyVars,  "let g : A -> A"]
simpleRules = ["r(x, y), r(y, z) -> r(x, z)", "r(x, y), r(y, x) -> False"]
simpleFormulas = map parseRule simpleRules

sampleState = Expression.getState $ map parseDeclaration simpleDeclarations

wowo = Expression.unfoldInstance sampleState simpleFormulas

simpleProgram = map fromDeclaration simpleDeclarations ++ map fromFormula simpleRules
    where
        fromDeclaration d = Dec $ parseDeclaration d
        fromFormula f = For $ parseRule f

wukong = List.intercalate "; \n" $ map show $ Expression.getGamma simpleProgram

woochi = Expression.showSeveral $ map showPositive $ Solve.getModels simpleProgram 20
    where
        showPositive m = filter (\x -> not(List.isInfixOf "¬" $ show x)) $ map (\(x, y) -> x) $ filter (\(x, y) -> y == True) $ Map.toList m