--https://www.codewars.com/kata/5765870e190b1472ec0022a2/train/haskell
--Task
--You are at position [0, 0] in maze NxN and you can only move in one of the four cardinal directions (i.e. North, East, South, West). Return true if you can reach position [N-1, N-1] or false otherwise.
--
--Empty positions are marked ..
--Walls are marked W.
--Start and exit positions are empty in all test cases.

module Pathfinder1 where

import Data.Set (Set, empty, member, insert, fromList, (\\))

data Travel = Failed | Continue | Succeed
data Point = Pt Int Int deriving (Show, Read, Ord, Eq)

class Ord a => Walk a where
  (!?) :: a -> Set a -> (Set a, a)
  bump :: [String] -> a -> Travel
  redirect :: a -> Set a -> Set a

instance Walk Point where
  p !? ps = (insert p ps, if not $ member p ps then p else Pt (-1) (-1))
  bump ps (Pt i j)
    | i < 0 || j < 0 || i >= li || j >= lj = Failed
    | i == li - 1 && j == lj - 1 = Succeed
    | otherwise = if ps !! i !! j == '.' then Continue else Failed
    where (li, lj) = (length ps, length $ ps !! 0)
  redirect (Pt i j) = (\\) $ fromList $ [[Pt (i+x) j, Pt i (j+x)] | x <- [1,-1]] >>= id

pathFinder :: String -> Bool
pathFinder maze = snd $ finder (words maze) (empty, False) (Pt 0 0)

finder :: [String] -> (Set Point, Bool) -> Point -> (Set Point, Bool)
finder _ path@(_, True) _ = path
finder maze path p = case fmap (bump maze) (p !? fst path) of
  (npath, Failed) -> (npath, False)
  (npath, Succeed) -> (npath, True)
  (npath, Continue) -> foldl (finder maze) (npath, False) (redirect p npath)
