module Lexer (Token
            , tokenize) where

import Data.Char

----------
-- DATA --
----------

data Token = TInt Int
           | TChar Char
           | TLParen
           | TRParen
           | TLBracket
           | TRBracket
           | TQuack
           | TDefn
           | TSymbol String
  deriving (Show, Eq)

------------
-- LEXING --
------------

toToken :: String -> Token
toToken s = case s of
             "("  -> TLParen
             ")"  -> TRParen
             "["  -> TLBracket
             "]"  -> TRBracket
             "!"  -> TQuack
             ":=" -> TDefn
             x
              | isTInt x    -> asTInt x
              | isTChar x   -> asTChar x
              | isTSymbol x -> asTSymbol x
 where
   isTInt, isTChar, isTSymbol :: String -> Bool
   asTInt, asTChar, asTSymbol :: String -> Token

   isTInt              = and . map isDigit
   isTChar ('\'':c:[]) = True
   isTChar _           = False
   isTSymbol _         = True

   asTInt    = TInt . read
   asTChar   = TChar . head . tail
   asTSymbol = TSymbol

splitAround :: Char -> String -> [String]
splitAround c "" = []
splitAround c s@(x:xs)
  | x == c    = [[c]] ++ splitAround c xs
  | otherwise = let (p, r) = break (== c) s
                 in [p] ++ splitAround c r

splitAroundAll :: [Char] -> String -> [String]
splitAroundAll [] s     = [s]
splitAroundAll (c:cs) s = splitAround c s >>= splitAroundAll cs

tokenize :: String -> [Token]
tokenize s = words s >>= (map toToken 
                        . splitAroundAll ['(', ')', '[', ']'])
        