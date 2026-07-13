{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Set as Set
import qualified Data.Map as Dict
import Data.Set (union, difference, intersection, member)
import Data.List (intercalate)
import Data.Set (Set)
import Control.Parallel.Strategies (parListChunk, rdeepseq)

infixr 0 |>
(|>) :: (a -> b) -> a -> b
(|>) f a = f a

showow :: Show a => [[a]] -> String
showow xss = intercalate " " (map show xss)

bigUnion :: Ord a => [Set a] -> Set a
bigUnion sets = foldr union Set.empty sets

type Binding = Dict.Map String String

data Term = Leaf String
          | Attribute Term Term
          | Index Term [Term]
          | Operation Term Term Term
    deriving (Eq, Ord)

data Atom = Relation Term [Term]
          | Comparison Term Term Term
    deriving (Eq, Ord)

data Literal = Positive Atom
             | Negative Atom
    deriving (Eq, Ord)
                         
data Conjunction = Mono Literal
                 | Poly [Term] [Literal]
    deriving (Eq, Ord)

data Formula = Assertion [Conjunction]
             | Implication [Conjunction] [Conjunction]
             | Equivalence [Conjunction] [Conjunction]
             | Contradiction [Conjunction]
             | Disjunction [Conjunction]
    deriving (Eq, Ord)
 
instance Show Term where
    show (Leaf s) = s
    show (Attribute t1 t2) = (show t1) ++ "." ++ (show t2)
    show (Index t ts) = (show t) ++ "[" ++ (intercalate ", " (map show ts)) ++ "]"
    show (Operation t1 op t2) = show t1 ++ " " ++ show op ++ " " ++ show t2

instance Show Atom where
    show (Relation t ts) = (show t) ++ "(" ++ (intercalate ", " (map show ts)) ++ ")"
    show (Comparison t1 comp t2) = show t1 ++ " " ++ show comp ++ " " ++ show t2
 
instance Show Literal where
    show (Positive atom) = show atom
    show (Negative atom) = "¬" ++ show atom
 
instance Show Conjunction where
    show (Mono literal) = show literal
    show (Poly ts ls) = "for " ++ (intercalate ", " (map show ts)) ++ " : " ++ (intercalate ", " (map show ls))

class Ord a => Expression a where
    replace :: a -> Binding -> a
    collect :: a -> Set String -> Set String
    leaves :: a -> Set String
    isGround :: a -> Set String -> Bool
    collect expr vars = (leaves expr) `intersection` vars
    isGround expr vars = Set.null (collect expr vars)

-- Isnt collect just leaves intersection variables?
instance Expression Term where

    replace (Leaf name) binding = (Leaf (Dict.findWithDefault name name binding))
    
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
            t1Vars = (leaves t1)
            t2Vars = (leaves t2)
    
    leaves (Index t ts) = (leaves t) `union` (bigUnion tsVars)
        where
            tsVars = map (\x -> leaves x) ts
    
    leaves (Operation t1 op t2) = t1Vars `union` opVars `union` t2Vars
        where
            t1Vars = (leaves t1)
            opVars = (leaves op)
            t2Vars = (leaves t2)
			
-- order v[x] n : A[x].

{-
data Declaration = Constant [Term] Term       -- const a, b, c : A
                 | Order Term Term Term        -- order lala n : A
                 | Function Term [Term] Term  -- let f : A x B -> C
                 | Variable [Term] Term       -- var x, y : A 
                 | Assignment Term Term       -- let a.f = b
                 | Module Term [(Term, Term)] -- bind Module with { Module.A = Here.A }
                 | Parameters [Term]

-}

data Declaration = Constant [Term] Term       -- const a, b, c : A
                 | Order Term Term Term        -- order lala n : A
                 | Function Term [Term] Term  -- let f : A x B -> C
                 | Variable [Term] Term       -- var x, y : A 
                 | Assignment Term Term       -- let a.f = b
                 | Module Term [(Term, Term)] -- bind Module with { Module.A = Here.A }
                 | Parameters [Term]          -- params
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

data State = State {
    members	  :: Dict.Map Term [Term],     -- Map the name of a sort to a collection with its members
	variables :: [String],                 -- Keep track of all names that are intended to be interpreted as variables
	ranges    :: Dict.Map String Term,      -- Map a variable name (a string) to the name of the sort (a term) it ranges over 
	functions :: [Term],                   -- Keep track of the terms that are intended to be interpreted as functions
	images    :: Dict.Map Term Term,       -- Map the name of a function to the names of the sorts its image belongs to
    domains   :: Dict.Map Term [Term],     -- Map the name of a function to the names of the sorts whose product contains the functions' domain
	parameterValues :: Dict.Map String Int, -- Map parameters to their values
	indices   :: Dict.Map String [String]  -- Map the head of an indexed term to the signature of its indices
} deriving (Show)

{-

Errores:

War...ning?: All leaves in LHS of variable declaration must be variables 

Error: Term is declared as both a variable and a constant

Error: Signature mismatch
Variables must range over coindexed sorts

Error: Assignment to non-attribute term
lhs term must be an attribute in rules of the form let a.f = b
This loc instead has '', which is an [].
(line n)

Error: Duplicate signatures for function f
Functions are supposed to have only one declaration, with a domain and image
(line n and line m)

Error: Undefined Index

Error: Module not found

Error: Cyclic dependencies in declarations
The semantics for sets of declarations with cyclic dependencies are not well defined.
Here are the terms whose definitions are mutually reachable:
---

Warning: Uneven equivalence

order lala n : A.
order lala[x] : A[x].

const A[i, j] : Grid.

-}

termDependencies :: String -> [[String]] -> Set String
termDependencies x xss = (Set.fromList |> concat |> Set.toList |> allDependencies x xss) `difference` (Set.fromList reservedWords)
 
directDependencies :: (Eq a, Ord a) => a -> [[a]] -> Set.Set [a]
directDependencies x xss = Set.fromList $ filter (\y -> x `elem` y) xss

allDependencies :: (Eq a, Ord a) => a -> [[a]] -> Set.Set [a]
allDependencies x xss = accumulate (Set.singleton x) (Set.empty) xss Set.empty

accumulate :: (Eq a, Ord a) => Set.Set a -> Set.Set a -> [[a]] -> Set.Set [a] -> Set.Set [a]
accumulate queue visited sources accumulator | Set.null queue = accumulator
accumulate queue visited sources accumulator | otherwise =
    accumulate newQueue newVisited sources (accumulator `union` deps)
        where
            first = Set.findMin queue
            deps = directDependencies first sources
            new = Set.fromList $ concat (Set.toList deps)
            newVisited = visited `union` new `union` (Set.singleton first)
            newQueue = (queue `union` new) `difference` newVisited

-- tests

leftSquare = T.pack "["
rightSquare = T.pack "]"
dotSymbol = T.pack "."
colonSymbol = T.pack ":"
space = T.pack " "

reservedWords = words "var order let = params"

-- Falta sacar var y eso
removeReserved :: String -> String
removeReserved text = T.unpack |>  T.replace colonSymbol space |> T.replace leftSquare space |> T.replace rightSquare space |> T.replace dotSymbol space |> T.pack text


{- Las dependencias directas son diferentes para cada tipo de declaracion

var x : A. LHS depende de RHS
order vertex n : A. RHS depende de LHS.
let no tiene dependencias, creo? Ah, no, tanto la LHS como la RHS
pero esas son dependencias para instanciar la regla... no sé qué cambian
params m n no tiene dependencias.

Entonces para armar las direct dependencies es mejor usar un map... hmhh

-}

sampleDeclarations = [
    words |> removeReserved "x < A", --"var x : A",
    words |> removeReserved "t x < Type x ", --"var t[x] : Type[x]",
    words |> removeReserved "A < n", --"order vertex n : A",
    words |> removeReserved "Type x < m x", --"order vertex[x] m : Type[x]",
    words |> removeReserved "", --"let x.type = Type[x].first",
    words |> removeReserved "" -- "params m n"
    ]

expectedDependencies = []

-- Claim: a set of declarations is well defined if all leafs that are variables are well defined and all
-- nested variables depend only onn well defined variables
collectVariables :: [Declaration] -> Set String
collectVariables declarations = bigUnion |> map leaves |> filter isVariableDeclaration declarations

isVariableDeclaration :: Declaration -> Bool
isVariableDeclaration (Variable _ _) = True
isVariableDeclaration _ = False

collectDependencies :: [Declaration] -> Dict.Map String [String]
collectDependencies declarations = Dict.fromList []

sourceCandidates :: Declaration -> [(Term, Term)]
sourceCandidates (Constant consts sort) = map (\const -> (sort, const)) consts
sourceCandidates (Order prefix n sort) = [(sort, prefix), (sort, n)]
sourceCandidates (Function t ds im) = [(t, im)] ++ map (\d -> (t, d)) ds
sourceCandidates (Variable vars sort) = map (\v -> (v, sort)) vars
sourceCandidates (Assignment t1 t2) = [(t1, t2)]
sourceCandidates (Module moduleName bindings) = []


-- Not exactly: var t[i] : T[i]... ahh! Hmhh but there must be a similar case
-- let g[i, j]: Grid

-- topoSort :: [[a]] -> [[a]]

{-

bindings formula state = map Map.fromList toutesLesPaires
    where
        lesVars = Set.toAscList collect formula
        toutesLesPaires = map lesPaires leProducte
        lesPaires = (\x -> zip lesVars x)
        leProducte = sequence (map (\x -> getMembers state x)) lesVars

-}

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

bindingsTest = []

-- This assumes the operator is surrounded by spaces
splitLine operator line = halves
    where
        halves = (concat $ takeWhile notSeparator parts, concat $ tail $ dropWhile notSeparator parts)
        parts = words line
        notSeparator span = span /= operator

finalDot line = False
middleOperator line operator = False
isFalse line = False

noOperator line = False

dottedLine line = finalDot line

isAssertion line = wellFormed line && noOperator line

isImplication line = middleOperator line operator && wellFormed lhs && wellFormed operator
    where
        (lhs, rhs) = splitLine operator line
        operator = "->"
isEquivalence line = middleOperator line operator && wellFormed lhs && wellFormed operator
    where
        (lhs, rhs) = splitLine operator line
        operator = "<->"
isContradiction line = middleOperator line operator && wellFormed lhs && isFalse rhs
    where
        (lhs, rhs) = splitLine operator line
        operator = "->"

isDisjunction line = False

isProgram text = False

isTerm text = False

isTernary line = False
isRelation line = False

wellBalanced :: String -> Bool
wellBalanced line = keepCount line 0 0

keepCount :: String -> Int -> Int -> Bool
keepCount line n m | n < 0 = False
keepCount line n m | m < 0 = False
keepCount [] n m = True
keepCount (leftParen:line) n m = keepCount line (n + 1) m
keepCount (rightParen:line) n m = keepCount line (n - 1) m
keepCount (leftSquare:line) n m = keepCount line n (m + 1)
keepCount (rightSquare:line) n m = keepCount line n (m - 1)

-- devolver los 'cortes' donde los parentesis empiezan y terminan en 0
-- "a (b[i, j.k[c, j]], k.r[c.l[d[k].j]]), b (c, d)"

-- las comas son ignorables?
-- si las reemplazas por espacios si

coindexedSegments span = []

allHeaded line = False

wellFormed :: String -> Bool
wellFormed line = wellBalanced line && allHeaded line

-- variables state tiene que ser una lista entonces
{- 
assignments statement state = map makeAssignment product
    where
        variables = toList (collect statement (getVariables state))
        product = sequence |> map (\var -> retrieve var state) |> variables 
        makeAssignment xs = Map.fromList |> zip variables xs
-}

{-
retrieve :: String -> State -> [String]
retrieve variable state = Map.findWithDefault [] range (getMembers state)
    where
        range = Map.findWithDefault "welp" variable (getRanges state)
-}

{-

getAssignments :: Statement -> State -> [Map String String]
getAssignments statement state =

    where
        variables = toList (collect statement )

-}

-- variables state tiene que ser una lista entonces

assignments :: Expression a => a -> Dict.Map String [String] -> [String] -> [Dict.Map String String]
assignments statement ranges variables = map makeAssignment product
    where
        product = sequence |> map (\var -> retrieve var) |> variables
        makeAssignment xs = Dict.fromList |> zip variables xs
        retrieve var = Dict.findWithDefault [] var ranges


ground :: Expression a => a -> Dict.Map String [String] -> [String] -> [a]
ground expression ranges variables = map (\b -> bind b) allAssignments `using` parListChunk 1000 rdeepseq 
    where
        bind b = replace expression b
        allAssignments = assignments expression ranges variables

{-



-}

-- allHeaded -> every () has some p to the left and either , or EOL to the right

-- [(index, line)]

-- readFormula lines | cond = res

{-

Cómo aseguramos que los nombres de constante que introducimos con llamadas de modulos no sobreescriben nombres del programa principal?

una opcion es que tengan de prefijo un caracter que solo usas ahi. Emhh, veamos:

bindear un modulo agarra el programa del modulo, lo unfoldea, y...
y reemplaza en el modulo los nombres de los sorts por... los nombres que interfacean con el programa principal, los que vos queres usar

los predicados y nombres del modulo- se tienen que pisar con los del programa principal?
Creeria que si
Porque si lala.h : Number y lala.g : Number, yo quiero poder sumar lala.h y lala.g, porque son 'dos llamadas del mismo módulo'

capaz podemos ponerle unos exposing a los modulos mhhh

Name clash import warnings con los modulos - hah!

-}

data Statement = Dec Declaration
               | For Formula

{-

- {... build F ...}
- var x : F.
- let x : A -> B.

let A : A -> B
What's that even mean?



let x.f : A -> B

Ahi... como que x.f depende de x
Ehhh...

Entonces hay como una especie de orden
Y es el orden de las hojas?

AHHH
Ya se: es como que tenes que definir depends como una funcion de Expression -> [(String, String)]

Y ahí tiene más sentido:

en t.s, s depende de t (creo?)
en t[s], t depende de s
en t, t no depende de nada
en t o s, o depende de t y de s


const t : s ==> s depende de todo lo que t dependa, mas t
order t m : o ==> o depende de todo lo que t y m dependan, mas m y t
let t : ts -> s ==> 
var t : A.  ==> t depende de todo lo que t dependa, más A
let a.f = b ==> no es f... ni es a... es que a.f depende de todo lo que b dependa... raraso???
params n, m, k ==> no dependen de nada

ok hay declaraciones ground y declaraciones con variables
las ground vienen todas antes
pero no sabes si una declaracion es ground o no hasta que no sabes qué variables hay
pero las declaraciones de variables tienen un orden
la cantidad de hojas alcanza??
Ajaja... no, la cantidad de hojas yy la cantidad de variables
De 'nombres' en el 'nivel anterior' que sean variables
Huhhh...

(1, 0) es el primero
(2, 1) viene antes que (2, 2)
Je, bien

Y como sabes si el orden de una declaracion o un termino es (n, m)?
Bueno, bien, empieza a tener sentido!


-}

{-



updateState [] state = state
updateState (d:ds) state = updateState ds newState
    where
        newState = stateSum (interpretation d state) state

getState declarations = updateState sortedDeclarations emptyState
    where
        sortedDeclarations = []

-}