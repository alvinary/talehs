{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# QuasiQuotes #-}
{-# Language FlexibleContexts #-}

module Expression where

import Prelude

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Set as Set
import qualified Data.Map as Dict
import Data.Set (union, difference, intersection, member)
import Data.List (intercalate, isInfixOf)
import Data.Set (Set)
import Data.Char (isDigit)
import Numeric (showBin)
import Prelude (putStrLn)

import Control.Parallel.Strategies (parListChunk, rdeepseq, using, NFData)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

infixr 0 |>
(|>) :: (a -> b) -> a -> b
(|>) f a = f a

-------------------------------------------------------------------------------------------
-- Definitions of distinguished terms that work as special constants and names of ---------
-- special relations, operations, and functions -------------------------------------------
-------------------------------------------------------------------------------------------

equalityComparison = Leaf "="

-------------------------------------------------------------------------------------------
-- Utility functions ----------------------------------------------------------------------
-------------------------------------------------------------------------------------------

showSeveral :: Show a => [[a]] -> String
showSeveral xss = intercalate "; \n" (map show xss)

showInLines xs = putStrLn |> intercalate "\n" (map show xs)

showThing t = show t

-- Map an integer to a list with its binary digits (in the usual order)
-- TODO: in which order?
binaryDigits :: Int -> [Int]
binaryDigits n = map readDigit (showBin n "")
    where
        readDigit d | d == '0' = 0
        readDigit d | d > '0' = 1

-- Minimum b such that 2**b >= n
logTwoCeiling :: Int -> Int
logTwoCeiling n = ceiling |> logBase 2 (fromIntegral n)

-- Self-explanatory
isSubstring :: String -> String -> Bool
isSubstring = isInfixOf

-- Check if a string contains only digits
isSequenceOfDigits :: String -> Bool
isSequenceOfDigits s = all isDigit s

-- Union of a list of sets
bigUnion :: Ord a => [Set a] -> Set a
bigUnion sets = foldr union Set.empty sets

-- Suppose you have a Dict whose values are lists.
-- Fuse adds all the elements in `new` (a list) to `map[key]`
fuse :: Ord k => k -> [v] -> Dict.Map k [v] -> Dict.Map k [v]
fuse key new map = Dict.insertWith (++) key new map

-- Set the value of all input keys to some input value
-- Esto podria ser un union de un fromlist y listo
split :: Ord k => [k] -> v -> Dict.Map k v -> Dict.Map k v
split [] val map = map
split (k:ks) val map = split ks val newMap
    where
        newMap = Dict.insert k val map

-- Show a term as a string
flatten :: Term -> String
flatten t = show t

-- Map a list of terms to a list of strings
massFlatten :: [Term] -> [String]
massFlatten ts = map flatten ts

-------------------------------------------------------------------------------------------
-- Type aliases ---------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

-- Used for substitutions

type Binding = Dict.Map String Term

-------------------------------------------------------------------------------------------
-- Types of expressions (terms and formulas) ----------------------------------------------
-------------------------------------------------------------------------------------------

data Term = Leaf String
          | Attribute Term Term
          | Index Term [Term]
          | Operation Term Term Term
    deriving (Eq, Ord, Generic, NFData)

data Atom = Relation Term [Term]
          | Comparison Term Term Term
    deriving (Eq, Ord, Generic, NFData)

data Literal = Positive Atom
             | Negative Atom
    deriving (Eq, Ord, Generic, NFData)

data Conjunction = Mono Literal
                 | Poly [Term] [Literal]
    deriving (Eq, Ord, Generic, NFData)

data Formula = Assertion [Conjunction]
             | Implication [Conjunction] [Conjunction]
             | Equivalence [Conjunction] [Conjunction]
             | Contradiction [Conjunction]
             | Disjunction [Conjunction]
    deriving (Eq, Ord, Generic, NFData)
 
instance Show Term where
    show (Leaf s) = s
    show (Attribute t1 t2) = (show t1) ++ "." ++ (show t2)
    show (Index t ts) = (show t) ++ "[" ++ (intercalate ", " (map show ts)) ++ "]"
    show (Operation t1 op t2) = show t1 ++ " " ++ show op ++ " " ++ show t2

instance Show Atom where
    show (Relation t ts) = show t ++ " (" ++ (intercalate ", " (map show ts)) ++ ")"
    show (Comparison t1 comp t2) = show t1 ++ " " ++ show comp ++ " " ++ show t2
 
instance Show Literal where
    show (Positive atom) = show atom
    show (Negative atom) = "¬" ++ show atom
 
instance Show Conjunction where
    show (Mono literal) = show literal
    show (Poly ts ls) = (intercalate ", " (map show ts)) ++ " | { " ++ (intercalate ", " (map show ls)) ++ " }"

instance Show Formula where
    show (Assertion conjuncts) = intercalate ", " (map show conjuncts)
    show (Contradiction conjuncts) = intercalate ", " (map show conjuncts) ++ " => False"
    show (Disjunction conjuncts) = intercalate " v " (map show conjuncts)
    show (Implication hypo conc) = intercalate ", " (map show hypo) ++ " => " ++ intercalate ", " (map show conc)
    show (Equivalence lhs rhs) = intercalate ", " (map show lhs) ++ " <=> " ++ intercalate ", " (map show rhs)

class Ord a => Expression a where
    replace :: a -> Binding -> a
    collect :: a -> Set String -> Set String
    leaves :: a -> Set String
    atoms :: a -> Set Atom
    isGround :: a -> Set String -> Bool
    collect expr vars = (leaves expr) `intersection` vars
    isGround expr vars = Set.null (collect expr vars)

instance Expression Term where

    replace term binding | Dict.member (show term) binding = Dict.findWithDefault (Leaf "error") (show term) binding

    replace (Leaf name) binding = Dict.findWithDefault (Leaf name) name binding
    
    replace (Attribute t1 t2) binding = (Attribute t1_ t2_)
        where
            t1_ = replace t1 binding
            t2_ = replace t2 binding
    
    replace (Index t ts) binding = (Index t_ ts_)
        where
            t_ = replace t binding
            ts_ = map (\x -> replace x binding) ts
    
    replace (Operation t1 op t2) binding = (Operation t1_ op_ t2_)
        where
            t1_ = replace t1 binding
            op_ = replace op binding 
            t2_ = replace t2 binding

    collect term vars | (show term) `member` vars = Set.singleton (show term)

    collect (Leaf name) vars | name `member` vars = Set.singleton name
    
    collect (Leaf name) vars | otherwise = Set.empty

    collect (Attribute t1 t2) vars = t1Vars `union` t2Vars
        where
            t1Vars = (collect t1 vars)
            t2Vars = (collect t2 vars)
    
    collect (Index t ts) vars = (collect t vars) `union` (bigUnion tsVars)
        where
            tsVars = map (\x -> collect x vars) ts
    
    collect (Operation t1 op t2) vars = t1Vars `union` opVars `union` t2Vars
        where
            t1Vars = (collect t1 vars)
            opVars = (collect op vars)
            t2Vars = (collect t2 vars)

    leaves (Leaf name) = Set.singleton name
    
    leaves (Attribute t1 t2) = t1Vars `union` t2Vars
        where
            t1Vars = leaves t1
            t2Vars = leaves t2
    
    leaves (Index t ts) = (leaves t) `union` (bigUnion tsVars)
        where
            tsVars = map (\x -> leaves x) ts
    
    leaves (Operation t1 op t2) = t1Vars `union` opVars `union` t2Vars
        where
            t1Vars = leaves t1
            opVars = leaves op
            t2Vars = leaves t2

    atoms term = Set.empty

instance Expression Atom where
    replace (Relation pred args) binding = (Relation pred_ args_)
        where
            pred_ = replace pred binding
            args_ = map (\x -> replace x binding) args
    replace (Comparison left comp right) binding = (Comparison left_ comp_ right_)
        where
            left_ = replace left binding
            right_ = replace right binding
            comp_ = replace comp binding
    leaves (Comparison left comp right) = leaves left `union` leaves right `union` leaves comp
    leaves (Relation pred args) = leaves pred `union` (bigUnion |> map (\a -> leaves a) args)
    atoms at = Set.singleton at

instance Expression Literal where
    replace (Positive at) binding = Positive at_
        where
            at_ = replace at binding
    replace (Negative at) binding = Negative at_
        where
            at_ = replace at binding
    leaves (Positive at) = leaves at
    leaves (Negative at) = leaves at
    atoms (Positive at) = Set.singleton at
    atoms (Negative at) = Set.singleton at

instance Expression Conjunction where
    replace (Mono literal) binding = Mono (replace literal binding)
    replace (Poly ranges literals) binding = Poly ranges_ literals_
        where
            ranges_ = map replace_ ranges
            literals_ = map replace_ literals
            replace_ x = replace x binding
    leaves (Mono literal) = leaves literal
    leaves (Poly ranges literals) = bigUnion rangeLeaves `union` bigUnion literalLeaves
        where
            rangeLeaves = map leaves ranges
            literalLeaves = map leaves literals
    atoms (Mono literal) = atoms literal
    atoms (Poly ranges literals) = bigUnion |> map atoms literals
    collect (Poly ranges literals) vars = Set.difference (bigUnion bodyVariables) (bigUnion rangeVariables)
        where
            rangeVariables = map (\x -> collect x vars) ranges
            bodyVariables = map (\x -> collect x vars) literals

instance Expression Formula where
    replace (Assertion conjuncts) binding = Assertion conjuncts_
        where
            replace_ x = replace x binding
            conjuncts_ = map replace_ conjuncts
    replace (Implication hypo conc) binding = Implication hypo_ conc_
        where
            replace_ x = replace x binding
            hypo_ = map replace_ hypo
            conc_ = map replace_ conc
    replace (Contradiction conjuncts) binding = Contradiction conjuncts_
        where
            replace_ x = replace x binding
            conjuncts_ = map replace_ conjuncts
    replace (Equivalence lhs rhs) binding = Equivalence lhs_ rhs_
        where
            replace_ x = replace x binding
            lhs_ = map replace_ lhs
            rhs_ = map replace_ rhs
    replace (Disjunction conjuncts) binding = Disjunction conjuncts_
        where
            replace_ x = replace x binding
            conjuncts_ = map replace_ conjuncts

    leaves (Assertion conjuncts) = bigUnion |> map leaves conjuncts
    leaves (Disjunction conjuncts) = bigUnion |> map leaves conjuncts
    leaves (Contradiction conjuncts) = bigUnion |> map leaves conjuncts
    leaves (Implication hypo conc) = (bigUnion |> map leaves hypo) `union` (bigUnion |> map leaves conc)
    leaves (Equivalence lhs rhs) = (bigUnion |> map leaves lhs) `union` (bigUnion |> map leaves rhs)
    
    atoms (Assertion conjuncts) = bigUnion |> map atoms conjuncts
    atoms (Disjunction conjuncts) = bigUnion |> map atoms conjuncts
    atoms (Contradiction conjuncts) = bigUnion |> map atoms conjuncts
    atoms (Implication hypo conc) = (bigUnion |> map atoms hypo) `union` (bigUnion |> map atoms conc)
    atoms (Equivalence lhs rhs) = (bigUnion |> map atoms lhs) `union` (bigUnion |> map atoms rhs)

------------------------------------------------------------------------------------------
-- Types of statements (rules and declarations) ------------------------------------------
------------------------------------------------------------------------------------------

data Declaration = Constant [Term] Term        -- const a, b, c : A
                 | Order Term Term Term        -- order lala n : A
                 | Function Term [Term] Term   -- let f : A x B -> C
                 | Variable [Term] Term        -- var x, y : A
                 | Assignment Term Term        -- let a.f = b
                 | Module Term [(Term, Term)]  -- bind Module with { Module.A = Here.A }
                 | Parameters [Term]           -- params
    deriving (Eq, Ord, Show)

instance Expression Declaration where

    replace (Constant ts t) binding = (Constant ts_ t_)
        where
            t_ = r t
            ts_ = map (\x -> r x) ts
            r x = replace x binding
    
    replace (Order t1 t2 t3) binding = (Order t1_ t2_ t3_)
        where
            t1_ = r t1
            t2_ = r t2
            t3_ = r t3
            r x = replace x binding

    replace (Function f dom img) binding = (Function f_ dom_ img_)
        where 
            f_ = r f
            dom_ = map (\x -> r x) dom
            img_ = r img
            r x = replace x binding

    replace (Variable ts t) binding = (Variable ts_ t_)
        where
            ts_ = map (\x -> r x) ts
            t_ = r t
            r x = replace x binding

    replace (Assignment t1 t2) binding = (Assignment t1_ t2_)
        where
            t1_ = r t1
            t2_ = r t2
            r x = replace x binding

    replace (Module t1 ps) binding = (Module t1_ ps_)
        where
            t1_ = r t1
            ps_ = map (\(x, y) -> (r x, r y)) ps
            r x = replace x binding

    replace (Parameters ts) binding = (Parameters ts_)
        where
            ts_ = map (\x -> r x) ts
            r x = replace x binding

    leaves (Constant ts t) =  leaves t `union` bigUnion (map leaves ts)
    leaves (Order t1 t2 t3) = leaves t1 `union` leaves t2 `union` leaves t3
    leaves (Function f dom img) = leaves f `union` bigUnion (map leaves dom) `union` leaves img
    leaves (Variable ts t) = leaves t `union` bigUnion (map leaves ts)
    leaves (Assignment t1 t2) = leaves t1 `union` leaves t2
    leaves (Module t ps) = leaves t `union` bigUnion (map (\(x, y) -> leaves x `union` leaves y) ps)
    leaves (Parameters ts) = bigUnion (map leaves ts)

    atoms declaration = Set.empty

-------------------------------------------------------------------------------------------

data Statement = Dec Declaration
               | For Formula

instance Show Statement where
        show (Dec d) = show d
        show (For f) = show f

-------------------------------------------------------------------------------------------
-- State and state updates ----------------------------------------------------------------
-------------------------------------------------------------------------------------------

data State = State {
    members	   :: Dict.Map Term [Term],      -- Map the name of a sort to a collection with its members
	variables  :: [String],                  -- Keep track of all names that are intended to be interpreted as variables
	ranges     :: Dict.Map String Term,      -- Map a variable name (a string) to the name of the sort (a term) it ranges over 
	functions  :: [Term],                    -- Keep track of the terms that are intended to be interpreted as functions
	images     :: Dict.Map Term Term,        -- Map the name of a function to the names of the sorts its image belongs to
    domains    :: Dict.Map Term [Term],      -- Map the name of a function to the names of the sorts whose product contains the functions' domain
	parameters :: Dict.Map Term Int,         -- Map parameters to their values
	indices    :: Dict.Map Term [Term],      -- Map the head of an indexed term to the signature of its indices
    values     :: Dict.Map (Term, Term) Term -- Map f t to f(t)
} deriving (Show)

emptyState = State Dict.empty [] Dict.empty [] Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty

-- Functions used to ensure evaluation terminates / there are no cyclic declarations -----

reservedWords = ["->", "let", "const", "var", "(", ")", "[", "]", ":", ",", ".", "False", "order", "=", "<"]

leftSquare = T.pack "["
rightSquare = T.pack "]"
dotSymbol = T.pack "."
colonSymbol = T.pack ":"
space = T.pack " "

removeReserved :: String -> String
removeReserved text = T.unpack |>  T.replace colonSymbol space |> T.replace leftSquare space |> T.replace rightSquare space |> T.replace dotSymbol space |> T.pack text

-- lala => [(t1, t2)] => deps

sampleDeclarations = [
    words |> removeReserved "A x",  --"var x : A",
    words |> removeReserved "x d ", --"var t[x] : Type[x]",
    words |> removeReserved "a d",  --"order vertex n : A",
    words |> removeReserved "d e",  --"order vertex[x] m : Type[x]",
    words |> removeReserved "f e",  --"let x.type = Type[x].first",
    words |> removeReserved "a g"   -- "params m n"
    ]

termDependencies :: String -> [[String]] -> Set String
termDependencies x xss = allDependencies x xss `difference` Set.fromList reservedWords
 
directDependencies :: (Eq a, Ord a) => a -> [[a]] -> Set a
directDependencies e deps = Set.delete e $ Set.fromList $ map head $ filter (\xs -> e `elem` xs) deps

allDependencies :: (Eq a, Ord a) => a -> [[a]] -> Set a
allDependencies x xss = accumulate (Set.singleton x) (Set.empty) xss Set.empty

accumulate :: (Eq a, Ord a) => Set a -> Set a -> [[a]] -> Set a -> Set a
accumulate queue visited sources accumulator | Set.null queue = accumulator
accumulate queue visited sources accumulator | otherwise =
    accumulate newQueue newVisited sources (accumulator `union` firstDeps)
        where
            first = Set.findMin queue
            firstDeps = directDependencies first sources
            newVisited = visited `union` (Set.singleton first)
            newQueue = (queue `union` firstDeps) `difference` newVisited


sourceCandidates :: Declaration -> [(Term, Term)]
sourceCandidates (Constant consts sort) = map (\const -> (sort, const)) consts
sourceCandidates (Order prefix n sort) = [(sort, prefix), (sort, n)]
sourceCandidates (Function t ds im) = [(t, im)] ++ map (\d -> (t, d)) ds
sourceCandidates (Variable vars sort) = map (\v -> (v, sort)) vars
sourceCandidates (Assignment t1 t2) = [(t1, t2)]
sourceCandidates (Module moduleName bindings) = []

collectAllVariables :: [Declaration] -> Set String
collectAllVariables declarations = bigUnion |> map leaves |> filter isVariableDeclaration declarations

isVariableDeclaration :: Declaration -> Bool
isVariableDeclaration (Variable _ _) = True
isVariableDeclaration _ = False

collectDependencies :: [Declaration] -> Dict.Map String [String]
collectDependencies declarations = Dict.fromList []

------------------------------------------------------------------------------------------

stateUpdate :: Declaration -> State -> State
stateUpdate (Constant ts t) state = state { members = fuse t ts (members state) }
stateUpdate (Variable ts t) state = state { ranges = split (massFlatten ts) t (ranges state), variables = (variables state) ++ newVariables }
    where
        newVariables = massFlatten ts -- en realidad, es cada nueva variable que creaste, con las sustituciones pertinentes
        allAssignments = []
stateUpdate (Order prefix size sort) state = addTotalOrder prefix size sort state
stateUpdate (Function f domain image) state = addFunction f domain image state
stateUpdate (Assignment (Attribute t f) s) state = state { values = Dict.insert (t, f) s (values state) }
-- stateUpdate (Parameters ts) state = state { parameters = Dict.fromList |> getParameters ts }
stateUpdate _ state = error "Undefined state update"

-- Parameters with default values
-- Parameters with terminal values
-- Binding parameters to sort values

addTotalOrder :: Term -> Term -> Term -> State -> State
addTotalOrder prefix size sort state = state { members = newMembers, values = newValues }
    where
        newMembers = fuse sort totalOrder (members state)
        newValues = Dict.union (values state) valuesMap     -- pisa a la izq? -- deberiamos tirar una warning si se pisan values?
        totalOrder = map (\t -> Index prefix [t]) rangeTerms
        rangeTerms = map (\i -> Leaf |> show i) [1..sizeValue]
        sizeValue = asSize size state
        valuesMap = Dict.fromList |> map makeValue [1..(sizeValue - 1)]  -- 
        makeValue i = ((Index prefix [Leaf |> show i], next), Index prefix [Leaf |> show |> i + 1])    -- (i, next) = i + 1
        next = Leaf "next"

addFunction :: Term -> [Term] -> Term -> State -> State
addFunction f domain image state = state { images = newImages, domains = newDomains, functions = newFunctions }
    where
        newImages = Dict.insert f image (images state)
        newDomains = Dict.insert f domain (domains state)
        newFunctions = f:(functions state)

-- If a term has an interpretation as an integer (it is either a sequence of digits or a parameter),
-- return that integer. Otherwise, raise an exception.
asSize :: Term -> State -> Int
asSize (Leaf a) state | isSequenceOfDigits a = read a
asSize term state | Dict.member term (parameters state) = Dict.findWithDefault 0 term (parameters state)
asSize _ state = error "In order to be converted to a size, a term must be either a sequence of digits or a parameter."

---------------------------------------------------------------------------------------
-- Encodings and Formulas -------------------------------------------------------------
---------------------------------------------------------------------------------------

-- Encode the unique name assumption for a list of terms as a list of formulas

uniqueNameAssumption :: [Term] -> [Formula]
uniqueNameAssumption terms = concat |> map unaFormula |> sequence [terms, terms]
    where
        unaFormula [x, y] | x == y = [areEqual x y] ++ negationConstraints x y
        unaFormula [x, y] | x /= y = [areNotEqual x y] ++ negationConstraints x y
        equalityAtom x y = Comparison x equalityComparison y
        negationConstraints x y = eitherFrom (Positive |> equalityAtom x y) (Negative |> equalityAtom x y)
        areEqual x y = Assertion [Mono |> Positive |> equalityAtom x y]
        areNotEqual x y = Assertion [Mono |> Negative |> equalityAtom x y]


-- Encode a function (a relation that's injective and surjective)

encodeFunction :: Term -> [[Term]] -> [Term] -> [Formula]
encodeFunction functionName domain image = encodeValues ++ encodeBounds 
    where
        encodeValues = concat |> (map (\args -> encodeDomainElement functionName args image) domain `using` parListChunk 1000 rdeepseq)
        encodeBounds = forbidOffBounds functionName firstOffBounds lastOffBounds domain imageSize -- TODO: los nombres están swappeados
        imageSize = length image
        firstOffBounds = imageSize                    -- TODO: Oboe
        lastOffBounds = 2 ^ (logTwoCeiling imageSize) -- TODO: Oboe. eso, eso +1, o eso -1?
        
encodeDomainElement :: Term -> [Term] -> [Term] -> [Formula]
encodeDomainElement functionName arguments image = encodeBits ++ encodeAssignment
    where
        encodeBits = bitConstraints functionName arguments imageSize --- Each element must be associated with a bit vector...
        encodeAssignment = concat |> map (\(index, value) -> valueFormulas functionName arguments value index imageSize) |> enumerate image  -- Todo: 0 indexed or 1 indexed? 
        imageSize = length image

forbidIndexBits :: Term -> [Term] -> Int -> Int -> Formula
forbidIndexBits f elem index max = Contradiction |> elementBits f elem index max

-- Given a function, an element, its index, and the index of the last element in the
-- domain, compute the list of conjucts [bits[f, 1] (a,  b_1), ..., bits[f, i] (a,  b_i), ..., bits[f, n] (a, b_n)]
-- TODO: calculas newxTwoPower ochenta veces, pasalo de parámetro culeah
elementBits :: Term -> [Term] -> Int -> Int -> [Conjunction]
elementBits f elem n max = map bitAtom |> zip elementBits [0..w]
    where
        bitAtom (bit, index) = Mono |> Positive |> Relation (fBitsPredicate index) (elem ++ [bit])
        fBitsPredicate i = Index bitsTerm [f, indexTerm i]
        elementArguments = elem ++ elementBits
        bitsTerm = Leaf "bits"
        indexTerm i = Leaf |> show i
        elementBits = padding ++ (map (\x -> Leaf |> show x) |> binaryDigits n)
        w = logTwoCeiling max
        paddingSize = w - (length |> binaryDigits n)
        padding = map (\x -> Leaf "0") [0..paddingSize]

-- valueFormulas functionName DomainSorts ImageSort elementIndex ImageSize
-- TODO: Should this be 'just' the conjunct, or also a formulas for 'wrong bit -> not this value'?
--       Like, for all i, bit [f, i] (arts, b.bits[i].flip) -> not f (args, b).
valueFormulas :: Term -> [Term] -> Term -> Int -> Int -> [Formula]
valueFormulas f as b i m = [Implication bitsConjunct valueConjunct, Implication valueConjunct bitsConjunct] ++ atomOrNot
    where
        valueAtom = Relation f |> as ++ [b]
        valueConjunct = [Mono |> Positive |> valueAtom]
        bitsConjunct = (elementBits f as i m)
        atomOrNot = eitherFrom (Negative |> valueAtom) (Positive |> valueAtom)

-- `bitConstraints` ensures each tuple in the domain has either zero or one as value for each bit
-- These are necessary just once per domain element, not per `(domain element, image element)` pair 
bitConstraints :: Term -> [Term] -> Int -> [Formula]
bitConstraints f args m = concat |> map bitFormulas bitIndices
    where
        bitFormulas index = eitherFrom (bitPredicate index valueZero) (bitPredicate index valueOne)
        bitIndices = map (\i -> Leaf |> show i) [1..logM] -- TODO: oboe
        indexTerm index = Index (Leaf "bits") [f, index]
        valueZero = [(Leaf "0")]
        valueOne = [(Leaf "1")]
        bitPredicate bitIndex bitValue = Positive |> Relation (indexTerm bitIndex) (args ++ bitValue)
        logM = logTwoCeiling m

-- `from` is the first forbidden value, `to` is the last
forbidOffBounds :: Term -> Int -> Int -> [[Term]] -> Int -> [Formula]
forbidOffBounds f from to domain imageSize = [forbidIndexBits f elem index imageSize | elem <- domain, index <- [from..to]]
        

------------------------------------------------------------------------------------------

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate xs = enn 0 xs

enn :: Int -> [a] -> [(Int, a)]
enn _ [] = []
enn n (x:xs) = (n, x):(enn (n + 1) xs)

------------------------------------------------------------------------------------------

-- Encode the xor of two literals as a list of formulas
eitherFrom :: Literal -> Literal -> [Formula]
eitherFrom phi psi = [Disjunction both, Contradiction both]
    where
        phi_ = Mono |> phi
        psi_ = Mono |> psi
        both = [phi_, psi_]

------------------------------------------------------------------------------------------
-- Variables, ground vs non-ground formulas, and assignments -----------------------------
------------------------------------------------------------------------------------------

grounding :: (Expression a, NFData a) => a -> State -> [a]
grounding expression state | check expression = [expression]
    where
        check expression_ = isGround expression_ stateVariables
        stateVariables = Set.fromList |> variables state      
grounding expression state | otherwise = concat |> map (\x -> grounding x state) (unpackAndGround expression state stateVariables)
    where
        check expression_ = isGround expression_ stateVariables
        stateVariables = Set.fromList |> variables state

emptyTerm :: Term -- TODO: parser must reject empty terms. Ensure it does, ensure that's known.
emptyTerm = (Leaf "")

-- Do the 'partial' frounding without having to check every time if the result is ground. TODO: is this really more efficient?
unpackAndGround :: (Expression a, NFData a) => a -> State -> Set String -> [a]
unpackAndGround expression state stateVars = groundingStep expression allRanges expressionVariables
    where
        expressionVariables = Set.toList |> collect expression stateVars
        allRanges = Dict.fromList |> map (\v -> (v, getVar v)) expressionVariables
        getVar = retrieve (ranges state) (members state)

--------------------------------------------------------------------------------------------------------------

retrieve :: Dict.Map String Term -> Dict.Map Term [Term] -> (String -> [Term])
retrieve ranges_ members_ = \var -> (Dict.findWithDefault [] (Dict.findWithDefault emptyTerm var ranges_) members_)

--------------------------------------------------------------------------------------------------------------

groundingStep :: (Expression a, NFData a) => a -> Dict.Map String [Term] -> [String] -> [a]
groundingStep expression ranges variables = map bind allAssignments `using` parListChunk 1000 rdeepseq
    where
        bind b = replace expression b
        allAssignments = assignments expression ranges variables

assignments :: Expression a => a -> Dict.Map String [Term] -> [String] -> [Dict.Map String Term]
assignments expression ranges variables = map makeAssignment product
    where
        product = sequence |> map (\var -> retrieve_ var) |> variables
        makeAssignment xs = Dict.fromList |> zip variables xs
        retrieve_ var = Dict.findWithDefault [] var ranges

---------------------------------------------------------------------------------------------------------------
--- Map Lists of Declarations to States, Map States and Rules to Ground Formulae ------------------------------
---------------------------------------------------------------------------------------------------------------

getState :: [Declaration] -> State
getState [] = emptyState
getState (d:declarations) = stateUpdate d (getState declarations)

unfoldInstance :: State -> [Formula] -> [Formula]
unfoldInstance state rules = allFunctionEncodings ++ allRuleGroundings ++ unaEncoding ++ negationEncoding
    where
        allFunctionEncodings = encodeAllFunctions state
        allRuleGroundings = concat |> map (\x -> grounding x state) rules
        unaEncoding = []
        negationEncoding = encodeNegation state rules

encodeAllFunctions :: State -> [Formula]
encodeAllFunctions state = concat |> (map encodeF functionNames `using` parListChunk 1000 rdeepseq)
    where
        functionDomains = domains state 
        functionNames = functions state
        functionImages = images state
        encodeF f = encodeFunction f (getDomain f) (getImage f)
        sortMembers s = Dict.findWithDefault [] s (members state)
        getImage f = sortMembers |> Dict.findWithDefault (Leaf "") f (images state)
        getDomain f = sequence |> map sortMembers |> Dict.findWithDefault [] f (domains state)

encodeNegation :: State -> [Formula] -> [Formula]
encodeNegation state rules = concat |> map negationClauses allAtoms
    where
        allAtoms = Set.toList |> bigUnion |> map atoms rules
        groundAtoms atom = grounding atom state
        negationClauses atom = concat |> map groundNegation |> groundAtoms atom
        groundNegation atom = eitherFrom (Positive atom) (Negative atom)

--------------------------------------------------------------------------------------
-- Map Programs to their Models ------------------------------------------------------
--------------------------------------------------------------------------------------

getDeclarations :: [Statement] -> [Declaration]
getDeclarations [] = []
getDeclarations ((Dec d):statements) = d:(getDeclarations statements)
getDeclarations (_:statements) = getDeclarations statements

getRules :: [Statement] -> [Formula]
getRules [] = []
getRules ((For f):statements) = f:(getRules statements)
getRules (_:statements) = getRules statements

programState :: [Statement] -> State
programState program = getState |> getDeclarations |> program

--------------------------------------------------------------------------------------

getGamma :: [Statement] -> [Formula]
getGamma program = unfoldInstance state rules
    where
        state = programState program
        rules = getRules program

---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------
-- Errors and Warnings ----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Tests ---------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------
--- TODOS y preguntas -----------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------
-- CASOS DE ERROR -----------------------------------------------------
-----------------------------------------------------------------------

-- ERROR DE SINTAXIS

-- DECLARACIONES CÍCLICAS

-- DECLARACIÓN DE VARIABLE INCONSISTENTE

-- DECLARACIÓN DE FUNCIÓN INCONSISTENTE (mismo dominio, distinta imagen)

-- MÓDULO NO ENCONTRADO

-- ATRIBUTO SIN DEFINICIÓN

-- TÉRMINO EN DECLARACIÓN DE ORDEN TOTAL NO ES UN NÚMERO

-- LHS DE DECLARACIÓN DE ASIGNACIÓN NO ES UN ATRIBUTO

-----------------------------------------------------------------------
-- ADVERTENCIAS -------------------------------------------------------
-----------------------------------------------------------------------

-- EQUIVALENCIA IMPAR

-- SORT VACÍO

-- GROUNDING VACÍO

-- ATRIBUTO SOBREESCRITO

-- RANGO DE VARIABLE ESTÁ VACÍO

-----------------------------------------------------------------------
-- REQUISITOS DE PREPROCESAMIENTO -------------------------------------
-----------------------------------------------------------------------

-- Sustitución de operaciones -----------------------------------------

-- Sustitución de símbolos especiales de comparaciones ----------------