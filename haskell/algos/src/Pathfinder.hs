--https://www.codewars.com/kata/5765870e190b1472ec0022a2/train/haskell
--Task
--You are at position [0, 0] in maze NxN and you can only move in one of the four cardinal directions (i.e. North, East, South, West). Return true if you can reach position [N-1, N-1] or false otherwise.
--
--Empty positions are marked ..
--Walls are marked W.
--Start and exit positions are empty in all test cases.

module Pathfinder1 where

import Data.Set (Set, empty, member, insert, fromList, (\\))

data Point = Pt Int Int deriving (Show, Read, Ord, Eq)

class Foldable f => Walk f where
--  (!?) :: Ord a => Point -> f [Char] -> (f a, Bool)
--  (!?) :: Ord a => a -> f a -> (f a, Bool)
  (!?) :: Point -> f Point -> (f Point, Bool)

instance Walk Set where
  p !? ps = (insert p ps, not $ member p ps)

instance Walk [] where
  (Pt i j) !? ps
    | i < 0 || j < 0 || i >= li || j >= lj = ([], False)
    | otherwise = fmap (=='.') $ ([], (ps !! i !! j))
    where
      (li, lj) = (length ps, length $ ps !! 0)

pathFinder :: String -> Bool
pathFinder = finder empty (Pt 0 0) . word

finder :: Set Point -> Point -> [String] -> (Set Point, Bool)
finder path p@(Pt i j) maze = fmap walk (p !? path)
  where
    walk False = False
    walk True = find ()

directions :: Point -> Set Point -> Set Point
directions (Pt i j) = (\\) $ fromList $ [[Pt (i+x) j, Pt i (j+x)] | x <- [1,-1]] >>= id

intercalate "\n" [".W...", ".W...", ".W.W.", "...W.", "...W."]