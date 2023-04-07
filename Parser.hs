module Parser (Program (..)
             , Environment
             , Definition
             , Stack
             , Symbol
             , Value (..)
             , parse) where

import Data.List.Extra

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
type Definition = (Symbol, [Value])

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
                           [[TSymbol name], tkns] -> (name, parseExpressions tkns)
                           _                      -> undefined

-- Parses all of the expressions in the list of tokens given
parseExpressions :: [Token] -> [Value]
parseExpressions = repeatedly parseExpression

-- Takes in a list of tokens, reads the first value, and then returns the remaining tokens
parseExpression :: [Token] -> (Value, [Token])
parseExpression []     = error "no expression to read"
parseExpression (t:ts) = case t of
                           TInt i    -> (VInteger i, ts)
                           TChar c   -> (VChar c, ts)
                           TLParen   -> parseFunction ts
                           TRParen   -> error "unexpected right parenthesis"
                           TLBracket -> parseStack ts
                           TRBracket -> error "unexpected right bracket"
                           TDefn     -> error "unexpected defintion in expression"
                           TQuack    -> (VQuack, ts)
                           TSymbol s -> (VSymbol s, ts)
  where
    parseFunction :: [Token] -> (Value, [Token])
    parseFunction = parseFnSt True []

    parseStack :: [Token] -> (Value, [Token])
    parseStack = parseFnSt False []

    -- Given if the thing being parsed is a function, an accumulator of values (in reverse), 
    -- and a list of tokens, this function produces the value read and the unread tokens 
    parseFnSt :: Bool -> [Value] -> [Token] -> (Value, [Token])
    parseFnSt isFn vs ts = case ts of
                             [] -> error $ "expected closing right " 
                                        ++ (if isFn then "parenthesis" else "bracket")
                             (t: ts')
                               | isFn     && (t == TRParen)   -> (VFunction $ reverse vs, ts')
                               | not isFn && (t == TRBracket) -> (VStack $ reverse vs, ts')
                             _ -> let (v, ts') = parseExpression ts in
                                      parseFnSt isFn (v : vs) ts' 

