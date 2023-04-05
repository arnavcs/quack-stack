module ListOperations where

import Control.Monad

-- splits a list around a passed value
-- ex: splitAround 'c' "crustacian" -> ["c", "rusta", "c", "ian"]
splitAround :: Eq a => a -> [a] -> [[a]]
splitAround c [] = []
splitAround c s@(x:xs)
  | x == c    = [c] : splitAround c xs
  | otherwise = let (p, r) = break (== c) s
                 in p : splitAround c r

-- splits a list around the multiple passed values
-- ex: splitAround ['c', 's'] "crustacian" -> ["c", "ru", "s", "ta", "c", "ian"]
splitAroundAll :: Eq a => [a] -> [a] -> [[a]]
splitAroundAll = foldr ((>=>) . splitAround) return

