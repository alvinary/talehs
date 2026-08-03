{
module Reader where

import Expression
import Parser
}

%name read                                -- Name of the function Happy will generate
%tokentype    { Parser.Token }            -- Type of the Start non-terminal
%error        { Parser.parseError }       -- Name of the function to call if an error occurs during parsing
%errorhandlertype explist


%right '.'
%nonassoc '<' '>' '=' '->' '<->' '|' '{' '}'
%right '+' '*'
%right ','
%left '+' '-'
%left '*' '/'
%right 'x'

%token
leaf     { Parser.TokenLeaf $$ }        
not      { Parser.TokenNot }
arrow    { Parser.TokenArrow }
iff      { Parser.TokenIff }
bottom   { Parser.TokenBottom }
const    { Parser.TokenConstant }
var      { Parser.TokenVariable }
let      { Parser.TokenLet }
module   { Parser.TokenModule }
params   { Parser.TokenParameters }
sep      { Parser.TokenSeparator }
order    { Parser.TokenOrder }
'|'      { Parser.TokenQuantify }
':'      { Parser.TokenColon }
';'      { Parser.TokenCross }
'.'      { Parser.TokenDot }
','      { Parser.TokenComma }
'['      { Parser.TokenOpenBrackets }
']'      { Parser.TokenCloseBrackets }
'('      { Parser.TokenOpenParenthesis }
')'      { Parser.TokenCloseParenthesis }
'{'      { Parser.TokenBegin }
'}'      { Parser.TokenEnd }
'<'      { Parser.TokenLess }
'>'      { Parser.TokenGreater }
'='      { Parser.TokenEquals }
'+'      { Parser.TokenPlus }
'*'      { Parser.TokenTimes }

%%

Program : Statement                            {  [$1] }
        | Statement sep Program                { $1:$3 }

Statement : Declaration                        { Expression.Dec $1 }
          | Rule                               { Expression.For $1 }

Declaration : const TermSequence ':' Term      { Expression.Constant      $2 $4 }
            | var TermSequence ':' Term        { Expression.Variable      $2 $4 }
            | let Term ':' Crosses arrow Term  { Expression.Function   $2 $4 $6 }
            | let Term '=' Term                { Expression.Assignment    $2 $4 }
            | params TermSequence              { Expression.Parameters       $2 }
            | order Term Term ':' Term         { Expression.Order      $2 $3 $5 }

Rule :  Conj arrow bottom                      { Expression.Contradiction     $1 }
     |  Conj arrow Conj                        { Expression.Implication    $1 $3 }
     |  Conj iff Conj                          { Expression.Equivalence    $1 $3 }
     |  Conj                                   { Expression.Assertion         $1 }

Comparison : '<'                               { $1 }
           | '>'                               { $1 }
           | '='                               { $1 }

Conj : Literal                                 { [Expression.Mono $1] }
     | Polyadic                                { [$1] }
     | Polyadic ',' Conj                       { $1:$3 }
     | Literal ',' Conj                        { (Expression.Mono $1):$3 }

Polyadic : TermPairSequence '|' '{' LiteralSequence '}' { Expression.Poly $1 $4 }

LiteralSequence : Literal                         { [$1]  }
                | Literal ',' LiteralSequence     { $1:$3 }
     
Literal : not Atom                                { Expression.Negative $2 }
        | Atom                                    { Expression.Positive $1 }

Atom : Term '(' TermSequence ')'        { Expression.Relation $1 $3                }
     | Term Comparison Term             { Expression.Comparison $1 (Leaf "<") $3   }

TermPairSequence : Term ':' Term                       { [($1, $3)] }
                 | TermPairSequence ',' Term ':' Term  { $1 ++ [($3, $5)] }

TermSequence : Term                     {    [$1]    }
             | TermSequence ',' Term    { $1 ++ [$3] }

Crosses       : Term                    {    [$1]    }
              | Crosses ';' Term        { $1 ++ [$3] }

Term  : Term '.' Term                   { Expression.Attribute $1 $3 }
      | Term '[' TermSequence ']'       { Expression.Index     $1 $3 }
      | leaf                            { Expression.Leaf         $1 }
      | '(' Term ')'                    { $2                         }
