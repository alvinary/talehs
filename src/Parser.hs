--------------------------------------------------------------------------------------------
-- Text, parsing and recognition -----------------------------------------------------------
--------------------------------------------------------------------------------------------

module Parser where

import qualified Data.Text as Text 
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified Data.Char as Char

-- The order is important, because substrings are replaced sequentially
-- The linear order is meant to be compatible with the substring partial order
-- if, for instance, <= came before <=>, <= would get replace with the suitable token (TokenLeq)
-- before <==>, 'ignoring' the intended token (TokenIff)

tokenShapes :: [(String, Token)]
tokenShapes = [("<=>", TokenIff),
               ("not", TokenNot), 
               ("->", TokenArrow),  
               ("False", TokenBottom), 
               ("let", TokenLet), 
               ("const", TokenConstant),
               ("var", TokenVariable),
               ("order", TokenOrder),
               ("params", TokenParameters),
               ("module", TokenModule),
               ("!", TokenNequals),
               ("=", TokenEquals),
               ("&", TokenVee),
               ("{", TokenBegin),
               ("}", TokenEnd),
               ("|", TokenQuantify),
               (":", TokenColon),
               (";", TokenCross),
               ("(", TokenOpenParenthesis),
               (")", TokenCloseParenthesis),
               ("[", TokenOpenBrackets),
               ("]", TokenCloseBrackets),
               (",", TokenComma),
               (".", TokenDot)]

infixr 0 |>
(|>) :: (a -> b) -> a -> b
(|>) f a = f a

infixr 0 +++
(+++) :: Text.Text -> Text.Text -> Text.Text
(+++) s t = Text.append s t

allTokens :: [Text.Text]
allTokens = map (\(x, _) -> Text.pack x) tokenShapes

tokensMap :: Map.Map Text.Text Token
tokensMap = Map.fromList |> map (\(x, y) -> (Text.pack x, y)) tokenShapes

readToken :: Text.Text -> Token
readToken t | Map.member t tokensMap = Map.findWithDefault TokenSeparator t tokensMap
readToken t | otherwise = TokenLeaf |> Text.unpack t

delimitAll :: Text.Text -> Text.Text
delimitAll text = replaceOrderly allTokens text

removeOthers :: Text.Text -> Text.Text
removeOthers text = Text.intercalate (Text.pack "\n") |> filter isStatement |> Text.lines |> text

isStatement s = (not |> Text.null s) && (not |> allWhitespace s) && (not |> Text.isPrefixOf commentMarker s)
    where
        commentMarker = Text.pack "--"
        allWhitespace s = Text.all Char.isSpace s

replaceOrderly :: [Text.Text] -> Text.Text -> Text.Text
replaceOrderly [] text = text
replaceOrderly (t:ts) text = replaceOrderly ts |> Text.replace t (oneSpace +++ t +++ oneSpace) text
    where
        oneSpace = Text.pack " "

splitTokens :: Text.Text -> [Text.Text]
splitTokens text = map Text.strip |> filter (\x -> x /= Text.empty) |> Text.splitOn oneSpace text
    where
        oneSpace = Text.pack " "

data Token  = TokenLeaf String
            | TokenComparison String
            | TokenOperation String
            | TokenNot
            | TokenArrow
            | TokenIff
            | TokenBottom
            | TokenDot
            | TokenComma
            | TokenOpenBrackets 
            | TokenCloseBrackets
            | TokenOpenParenthesis
            | TokenCloseParenthesis
            | TokenSmaller
            | TokenGreater
            | TokenEquals
            | TokenNequals
            | TokenPlus
            | TokenTimes
            | TokenLet
            | TokenConstant
            | TokenVariable
            | TokenOrder
            | TokenParameters
            | TokenModule
            | TokenCross
            | TokenColon
            | TokenSeparator
            | TokenQuantify
            | TokenBegin
            | TokenEnd
            | TokenLess
            | TokenVee
    deriving Show

showToken :: Token -> String
showToken (TokenLeaf s) = s
showToken (TokenComparison s) = s
showToken (TokenOperation s) = s
showToken (TokenNot) = "not"
showToken (TokenArrow) = "->"
showToken (TokenIff) = "<->"
showToken (TokenBottom) = "False"
showToken (TokenDot) = "."
showToken (TokenComma) = ","
showToken (TokenOpenBrackets) = "["
showToken (TokenCloseBrackets) = "]"
showToken (TokenOpenParenthesis) = "("
showToken (TokenCloseParenthesis) = ")"
showToken (TokenSmaller) = "<"
showToken (TokenGreater) = ">"
showToken (TokenNequals) = "!"
showToken (TokenEquals) = "="
showToken (TokenPlus) = "+"
showToken (TokenTimes) = "*"
showToken (TokenLet) = "let"
showToken (TokenConstant) = "const"
showToken (TokenVariable) = "var"
showToken (TokenOrder) = "order"
showToken (TokenParameters) = "params"
showToken (TokenModule) = "module"
showToken (TokenCross) = "x"
showToken (TokenColon) = ":"
showToken (TokenSeparator) = "|"
showToken (TokenQuantify) = "|"
showToken (TokenBegin) = "{"
showToken (TokenEnd) = "}"
showToken (TokenVee) = "or"
showToken (TokenLess) = "<"

parseError :: ([Token], [String]) -> Either ([Token], [String]) a
parseError = Left

tokenize :: Text.Text -> [Token]
tokenize text = map readToken |> splitTokens adaptedText
    where
        adaptedText = delimitAll text

redo = TIO.readFile
printo = TIO.putStrLn