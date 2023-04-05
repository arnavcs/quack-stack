module Parser where

import Data.List.Split

import Lexer

----------
-- DATA --
----------

-- Represents the entire program as a whole, consisting of an environment and expressions
data Program = Program Environment [Stack]
  deriving (Show, Eq)

-- An environment is a list of definitions
type Environment = [Definition]

-- A definition is identified by the symbol it is bound to, and the replacement to be made
data Definition = Definition Symbol [Value]
  deriving (Show, Eq)

-- A stack is a list of values
type Stack = [Value]

-- A symbol is the name of a "variable"
type Symbol = String

-- Any value is either an integer, a character, a symbol (a definition), 
-- a function (a composition of symbols that are grouped together), a quack, or a stack
data Value = VInteger Int
           | VChar Char
           | VSymbol Symbol
           | VQuack
           | VFunction [Value]
           | VStack Stack
  deriving (Show, Eq)

-------------
-- PARSING --
-------------

-- TODO: write parseExpressions
parseExpressions :: [Token] -> [Value]
parseExpressions = undefined

-- Parses the list of lines to produce a program data
parse :: [[Token]] -> Program
parse = foldr parseLine (Program [] [])

-- Parses each line and adds to the environment and the list of expressions in the program
parseLine :: [Token] -> Program -> Program
parseLine line (Program env exprs)
  | isDefn line = Program (parseDefinition line : env) exprs
  | isExpr line = case parseExpressions line of
                    [VStack s] -> Program env (exprs ++ [s])
                    _          -> undefined
  | otherwise   = undefined
  where
    -- Is the line a defintion
    isDefn :: [Token] -> Bool
    isDefn = elem TDefn

    -- Is the line an expression
    isExpr :: [Token] -> Bool
    isExpr tknLst = head tknLst == TLBracket && last tknLst == TRBracket

-- Parses the given line and returns a definition
parseDefinition :: [Token] -> Definition
parseDefinition tknLst = case splitOn [TDefn] tknLst of
                           [[TSymbol name], tkns] -> Definition name $ parseExpressions tkns
                           _                      -> undefined

