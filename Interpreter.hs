module Interpreter (interpretProgram
                  , interpretExpression) where

import Lexer
import Parser

----------
-- DATA --
----------

type EvaluationState = (Stack, Stack)

------------------
-- INTERPRETING --
------------------

interpretProgram :: Program -> String
interpretProgram (Program env exprs) = unlines $ interpretExpression <$> exprs <*> pure env

interpretExpression :: Stack -> Environment -> String
interpretExpression stk = interpretExpression' ([], stk)
  where
    interpretExpression' :: EvaluationState -> Environment -> String
    interpretExpression' es@(_, []) _ = showEvaluationState es
    interpretExpression' es env       = interpretExpression' (stepEvaluationState es env) env

showEvaluationState :: EvaluationState -> String
showEvaluationState = undefined

stepEvaluationState :: EvaluationState -> Environment -> EvaluationState
stepEvaluationState = undefined

