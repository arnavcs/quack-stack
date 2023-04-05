module Lexer (Token (..)
            , tokenize) where

import Data.Char
import Control.Monad

import ListOperations

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

-- returns the token equivalent of a given string granted that the string has no whitespace
toToken :: String -> Token
toToken s = case s of
             "("  -> TLParen
             ")"  -> TRParen
             "["  -> TLBracket
             "]"  -> TRBracket
             "!"  -> TQuack
             ":=" -> TDefn
             x
              | isTInt x  -> asTInt x
              | isTChar x -> asTChar x
              | otherwise -> TSymbol x
 where
   isTInt, isTChar :: String -> Bool
   asTInt, asTChar :: String -> Token

   isTInt              = all isDigit
   isTChar ['\'', c] = True
   isTChar _           = False

   asTInt    = TInt . read
   asTChar   = TChar . head . tail

-- returns a list of tokens in the string for a single line
tokenizeLine :: String -> [Token]
tokenizeLine = map toToken . (splitAroundAll "()[]" <=< words)

-- returns a list of tokens for each line in the whole document
tokenize :: String -> [[Token]]
tokenize = map tokenizeLine . lines 

