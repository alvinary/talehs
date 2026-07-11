--------------------------------------------------------------------------------------------
-- Text, parsing and recognition -----------------------------------------------------------
--------------------------------------------------------------------------------------------

module Parser where

import qualified Data.Text as Text 
import qualified Data.Map as Map

-- The order is important, because substrings are replaced sequentially
-- The linear order is meant to be compatible with the substring partial order
-- if, for instance, <= came before <=>, <= would get replace with the suitable token (TokenLeq)
-- before <==>, 'ignoring' the intended token (TokenIff)

tokenShapes :: [(String, Token)]
tokenShapes = [(".\n", TokenSeparator),
               ("<=>", TokenIff),
               ("not", TokenNot), 
               ("->", TokenArrow),  
               ("False", TokenBottom), 
               ("let", TokenLet), 
               ("const", TokenConstant),
               ("var", TokenVariable),
               ("order", TokenOrder),
               ("params", TokenParameters),
               ("module", TokenModule),
               (":", TokenColon),
               (";", TokenCross),
               ("(", TokenOpenParenthesis),
               (")", TokenCloseParenthesis),
               ("[", TokenOpenBrackets),
               ("]", TokenCloseBrackets),
               (",", TokenComma)]

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
    deriving Show

parseError = error "Parse error"

tokenize :: Text.Text -> [Token]
tokenize text = map readToken |> splitTokens adaptedText
    where
        adaptedText = delimitAll text