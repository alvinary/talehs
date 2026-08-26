import qualified Solve
import Expression
import Reader
import Parser
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

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
simpleRules = ["r(x, y), r(y, z) -> r(x, z)", "r(x, y), r(y, x) -> False", "m(a) -> w : A | { p ( x , w ) }", "m (a)"]
simpleFormulas = map parseRule simpleRules

sampleState = Expression.getState $ map parseDeclaration simpleDeclarations

wowo = Expression.unfoldInstance sampleState simpleFormulas

lelor = simpleFormulas !! 2

lolodor = (wobtain leForm, Expression.bindAny (gobtain leForm) (Expression.members sampleState) $ Map.fromList [("x", (Expression.Leaf "a"))])
    where
        leForm = simpleFormulas !! 2
        wobtain (Implication a b) = obtain $ head b
        wobtain _ = error "Was expecting an implication"
        gobtain (Implication a b) = head b
        obtain (Poly head body) = Set.toList $ bigUnion $ map (\(x, y) -> leaves x) head
        obtain _ = []

simpleProgram = map fromDeclaration simpleDeclarations ++ map fromFormula (take 2 simpleRules)
    where
        fromDeclaration d = Dec $ parseDeclaration d
        fromFormula f = For $ parseRule f

wukong = List.intercalate "; \n" $ map show $ Expression.getGamma simpleProgram

woochi = Expression.showInLines $ map showPositive $ Solve.getModels simpleProgram 20
    where
        showPositive m = filter (\x -> not(List.isInfixOf "¬" $ show x)) $ map (\(x, y) -> x) $ filter (\(x, y) -> y == True) $ Map.toList m