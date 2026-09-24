{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# QuasiQuotes #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- vitabiosa

import Expression
import qualified TestSolve
import qualified Parser

testTokens = Parser.tokenize "let t[x, y].down = t[x, y.next]"

testDeclarations :: [Declaration]
testDeclarations = map TestSolve.parseDeclaration ["order a 15 : A", 
                                                   "var x, y : A",
                                                   "const t[x, y] : T",
                                                   "let f : A -> A",
                                                   "let t[x, y].down = t[x, y.next]",
                                                   "let t[x, y].right = t[x.next, y]",
                                                   "let t[x, y].down.up = t[x, y]"]
testState = applyDeclarations $ reverse testDeclarations