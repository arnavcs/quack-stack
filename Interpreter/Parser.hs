module Parser where

import Lexer

----------
-- DATA --
----------

-- Represents the entire program as a whole
newtype Program = Program [ProgramComponent]
  deriving (Show, Eq)

-- A singluar line of code. It can represent a definition or a stack
data ProgramComponent = PCDefinition Definition
                      | PCStack Stack
  deriving (Show, Eq)

-- A definition is identified by the name it is bound to, and the replacement to be made
data Definition = Definition String [Value]
  deriving (Show, Eq)

-- A stack is a list of values
newtype Stack = Stack [Value]
  deriving (Show, Eq)

-- Any value is either an integer, a character, a symbol (a definition), 
-- a function (a composition of symbols that are grouped together), a quack, or a stack
data Value = VInteger Int
           | VChar Char
           | VSymbol String
           | VQuack
           | VFunction [Value]
           | VStack Stack
  deriving (Show, Eq)

-------------
-- PARSING --
-------------
