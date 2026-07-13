import Expression
import Reader
import Parser
import qualified Data.Text as Text
import qualified Data.Map as Map

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

abcConstants = "const a, b, c, d, e, f : A"
xyVars = "var x, y, z : A"

checkFunctions functionDeclaration functionName state = allInDomain && boundsForbidden
    where
        allInDomain = False
        boundsForbidden = False 

simpleDeclarations = [abcConstants, xyVars]
simpleRules = ["r(x, y), r(y, z) -> r(x, z)", "r(x, y), r(y, x) -> False"]
simpleFormulas = map parseRule simpleRules

sampleState = Expression.getState $ map parseDeclaration simpleDeclarations

wowo = Expression.unfoldInstance sampleState simpleFormulas

{-
forbiddenStuff = intercalate "\n" [show sampleProhibitionA, show sampleProhibitionB, show sampleProhibitionC]

sampleGrounding = intercalate "\n" (map show groundedStuff)

hmhh = length groundedStuff == length sampleSort ^ 2

sampleProhibitionA = forbidIndexBits (Leaf "f") [(Leaf "a")] 4 7
sampleProhibitionB = forbidIndexBits (Leaf "f") [(Leaf "a")] 4 4
sampleProhibitionC = forbidIndexBits (Leaf "f") [(Leaf "a")] 6 12 

sampleLeaves = leaves rendy
sampleCollection = collect rendy (Set.fromList ["i", "j"])

sampleElementBitsA = elementBits (Leaf "f") [(Leaf "a")] 0 7
sampleElementBitsB = elementBits (Leaf "f") [(Leaf "a")] 3 7
sampleElementBitsC = elementBits (Leaf "f") [(Leaf "a")] 7 7
sampleElementBitsD = elementBits (Leaf "f") [(Leaf "a")] 0 8
sampleElementBitsE = elementBits (Leaf "f") [(Leaf "a")] 3 8
sampleElementBitsF = elementBits (Leaf "f") [(Leaf "a")] 7 8
sampleElementBitsG = elementBits (Leaf "f") [(Leaf "a")] 8 8

sampleBitConstraintsA = bitConstraints (Leaf "f") [(Leaf "a"), (Leaf "b")] 4
sampleBitConstraintsB = bitConstraints (Leaf "f") [(Leaf "a"), (Leaf "b")] 8
sampleBitConstraintsC = bitConstraints (Leaf "f") [(Leaf "a")] 8

expectedDependencies = []

pepino = (Leaf "pepino")
color = (Leaf "color")
tone = (Leaf "tone")
firstIndex = (Leaf "i")
secondIndex = (Leaf "j")
preceq = (Leaf "≼")
wendy = (Negative (Relation color [pepino, firstIndex]))
bendy = (Negative (Relation tone [pepino, firstIndex]))
rendy = (Poly [firstIndex, secondIndex] [wendy, bendy])

tepino = termDependencies "t" sampleDeclarations
xepino = termDependencies "x" sampleDeclarations
mepino = termDependencies "m" sampleDeclarations
aepino = termDependencies "A" sampleDeclarations

bindingsTest = [] -}