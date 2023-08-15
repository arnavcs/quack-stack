module Interpreter where

----------------
-- DATA TYPES --
----------------

type CleanStack 
    = [CleanTerm]

type DirtyStack
    = [DirtyTerm]

data StackZipper
    = StackZipper CleanStack DirtyStack
  deriving (Show, Eq)

type Routine 
    = [DirtyTerm] 

data PrimOp
    = Add
    | Neg
    | Not
    | And
    | Or
    | Compose
    | Push
    | Pop
  deriving (Show, Eq)

data CleanTerm 
    = CInt Int
    | CChar Char
    | CBool Bool
    | CStack CleanStack
    | CRoutine Routine
    | CPrimOp PrimOp
  deriving (Show, Eq)

data DirtyTerm 
    = DInt Int
    | DChar Char
    | DBool Bool
    | DStack DirtyStack
    | DRoutine Routine
    | DPrimOp PrimOp
    | DQuack
  deriving (Show, Eq)

-------------------------------
-- EXPRESSION INTERPRETATION --
-------------------------------

applyPrimOp :: PrimOp -> StackZipper -> StackZipper
applyPrimOp p (StackZipper cs ds)
    = StackZipper 
      (case p of
         Add -> (case cs of
                  (CInt x : CInt y : cs') -> CInt (x + y) : cs'
                  _ -> error "bad use of addition")
         Neg -> (case cs of 
                  (CInt x : cs') -> CInt (-x) : cs'
                  _ -> error "bad use of negation")
         Not -> (case cs of 
                  (CBool x : cs') -> CBool (not x) : cs'
                  _ -> error "bad use of boolean not")
         And -> (case cs of 
                  (CBool x : CBool y : cs') -> CBool (x && y) : cs'
                  _ -> error "bad use of boolean and")
         Or -> (case cs of 
                 (CBool x : CBool y : cs') -> CBool (x || y) : cs'
                 _ -> error "bad use of boolean or")
         Compose -> (case cs of
                      (x : y : cs') 
                        -> CRoutine ((case y of 
                                        CRoutine o -> o
                                        CPrimOp o  -> [DPrimOp o, DQuack]
                                        _          -> error "bad use of composition") 
                                  ++ (case x of
                                        CRoutine o -> o
                                        CPrimOp o  -> [DPrimOp o, DQuack]
                                        _          -> error "bad use of composition")) 
                         : cs'
                      _ -> error "bad use of composition")
         Pop -> (case cs of 
                  (CStack (t : ts) : cs') -> t : CStack ts : cs'
                  _ -> error "bad use of composition")
         Push -> (case cs of
                   (t : CStack ts : cs') -> CStack (t : ts) : cs'
                   _ -> error "bad use of composition"))
      ds

cleanStackStep :: StackZipper -> StackZipper
cleanStackStep s@(StackZipper cs []) = s
cleanStackStep (StackZipper cs (DQuack : ds))
    = case cs of
        (CRoutine r : cs') -> StackZipper cs' (r ++ ds)
        (CPrimOp p  : cs') -> applyPrimOp p $ StackZipper cs' ds
        _                  -> error "quacked non-quackable value"
cleanStackStep (StackZipper cs (d : ds))
    = StackZipper 
      (case d of
        (DInt x)     -> CInt x : cs
        (DChar x)    -> CChar x : cs
        (DBool x)    -> CBool x : cs
        (DRoutine x) -> CRoutine x : cs
        (DPrimOp x)  -> CPrimOp x : cs
        (DStack x)   -> CStack (cleanStack x) : cs)
      ds

cleanStack :: DirtyStack -> CleanStack
cleanStack ds 
    = generate
      (StackZipper [] ds)
      cleanStackStep
      (\ (StackZipper _ xs) -> null xs)
      (\ (StackZipper xs []) -> reverse xs)
  where
    generate :: a -> (a -> a) -> (a -> Bool) -> (a -> b) -> b
    generate init step done final
        | done init = final init
        | otherwise = generate (step init) step done final

