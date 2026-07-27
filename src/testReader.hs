import Expression
import Parser
import Reader
import qualified Data.Text as Text

p = Parser.TokenLeaf "p"
q = Parser.TokenLeaf "q"
r = Parser.TokenLeaf "r"
s = Parser.TokenLeaf "s"

a = Parser.TokenLeaf "a"
b = Parser.TokenLeaf "b"
c = Parser.TokenLeaf "c"
i = Parser.TokenLeaf "i"
j = Parser.TokenLeaf "j"
x = Parser.TokenLeaf "x"
y = Parser.TokenLeaf "y"

op = Parser.TokenOpenParenthesis
cl = Parser.TokenCloseParenthesis
bop = Parser.TokenOpenBrackets
cop = Parser.TokenCloseBrackets

comma = Parser.TokenComma
arrow = Parser.TokenArrow

vergaExample = [Parser.TokenLeaf "r", Parser.TokenOpenParenthesis, Parser.TokenLeaf "wanano", Parser.TokenCloseParenthesis]
vergaShow = Expression.showThing (Reader.read vergaExample)

vergaExample2 = [a, Parser.TokenOpenBrackets, i, TokenComma, j, Parser.TokenCloseBrackets]
vargaShow2 = Expression.showThing (Reader.read vergaExample2)

vergaExample3 = [p, op] ++ vergaExample2 ++ [comma, b, cl]
vergaShow3 = Expression.showThing (Reader.read vergaExample3)

vergaShow1Alt = Expression.showThing $ Reader.read $ [r, op, a, cl]
vergaShow2Alt = Expression.showThing $ Reader.read $ [r, op, b, bop, a, cop, cl]
vergaShow3Alt = Expression.showThing $ Reader.read $ [r, op, b, bop, a, cop, cl, arrow, r, op, a, cl]
vergaShow4Alt = Expression.showThing $ Reader.read $ [p, op, a, comma, b, cl]
vergaShow7Alt = Expression.showThing $ Reader.read $ [p, op, a, comma, b, cl, comma, q, op, b, comma, a, cl, arrow, s, op, b, cl]

vergaShow4 = Expression.showThing $ Reader.read $ Parser.tokenize $ Text.pack "p (a, b) "
vergaShow5 = Expression.showThing $ Reader.read $ Parser.tokenize $ Text.pack "p (a, b[x ,  y]) -> False"
vergaShow6 = Expression.showThing $ Reader.read $ Parser.tokenize $ Text.pack "p(a ,  b),  q(b, a) -> s(b)"
vergaShow7 = Expression.showThing $ Reader.read $ Parser.tokenize $ Text.pack sampleProgram

sampleProgram = "p(a, b), q(b, a) -> s(b).\
                \const a : A.\
                \s(b), p(b) -> False"

toki = Parser.splitTokens $ Text.pack "p( a ,  b ) ,  q ( b, a ) -> s ( b )"
showTokens = show $ Parser.tokenize $ Text.pack "p( a ,  b ) ,  q ( b, a ) -> s ( b )"
vergaShow8 = Expression.showThing $ Reader.read $ Parser.tokenize $ Text.pack "p( a ,  b ) ,  q ( b, a ) -> s ( b )"

palulu = Text.pack "p(a, b),  q(b, a) -> s (b)"
lolo = Parser.delimitAll $ Text.pack "p(a, b),  q(b, a) -> s (b)"
molo = Expression.showThing $ Reader.read $ Parser.tokenize palulu

polili = Text.pack "p(a, b) v p(b, a) v q(a, b)"
molili = Text.pack "not parapa (aba, lala), not karaka (wewe) -> tarapa (aba, lala, wewe)"
molala = Expression.showThing $ Reader.read $ Parser.tokenize molili
polala = Expression.showThing $ Reader.read $ Parser.tokenize polili

readDeclaration :: String -> Expression.Declaration
readDeclaration text = (\(Expression.Dec x) -> x) $ parse text

readRule :: String -> Expression.Formula
readRule text = (\(Expression.For x) -> x) $ parse text

parse text = head $ Reader.read $ Parser.tokenize $ Text.pack $ text

ejemplines = [readDeclaration "order i 25 : A",
              readDeclaration "let f : A -> B",
              readDeclaration "let f : A ; A -> B",
              readDeclaration "var x, y : A",
              readDeclaration "const a[x] : A[x]"]

reglitas = [
    parse "p(x)",
    parse "p(x, y)",
    parse "p(t[x], t[y]) -> s(t)",
    parse "p(x), s(x, y) -> False",
    parse "{ p(x), q(x) }",
    parse "x | p(x, y) | ",
    parse "x | p(x, y) | "]

orderDeclarationByTokens = [Parser.TokenOrder, Parser.TokenLeaf "i", Parser.TokenLeaf "25", Parser.TokenColon, Parser.TokenLeaf "A"]
orderDeclarationFromTokens = Expression.showThing $ head $ Reader.read orderDeclarationByTokens