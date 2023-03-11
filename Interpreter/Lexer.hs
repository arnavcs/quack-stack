module Lexer (Token
            , tokenize) where

import Data.Char
import Data.List
import Control.Monad

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

-- splits a string around a passed character
-- ex: splitAround 'c' "crustacian" -> ["c", "rusta", "c", "ian"]
splitAround :: Char -> String -> [String]
splitAround c "" = []
splitAround c s@(x:xs)
  | x == c    = [c] : splitAround c xs
  | otherwise = let (p, r) = break (== c) s
                 in p : splitAround c r

-- splits a string around the multiple passed characters
-- ex: splitAround ['c', 's'] "crustacian" -> ["c", "ru", "s", "ta", "c", "ian"]
splitAroundAll :: [Char] -> String -> [String]
splitAroundAll = foldr ((>=>) . splitAround) return

-- returns a list of tokens in the string for a single line
tokenizeLine :: String -> [Token]
tokenizeLine = map toToken . (splitAroundAll "()[]" <=< words)

-- returns a list of tokens for each line in the whole document
tokenize :: String -> [[Token]]
tokenize = map tokenizeLine . lines 

