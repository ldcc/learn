-- In this kata, you will write a function that returns the positions and the values
-- of the "peaks" (or local maxima) of a numeric array.
--
-- For example, the array arr = [0, 1, 2, 5, 1, 0] has a peak at position 3 with a
-- value of 5 (since arr[3] equals 5).
--
--The output will be returned as an object with two properties: pos and peaks. Both
-- of these properties should be arrays. If there is no peak in the given array,
-- then the output should be {pos: [], peaks: []}.
--
-- Example: pickPeaks([3, 2, 3, 6, 4, 1, 2, 3, 2, 1, 2, 3]) should return {pos: [3, 7],
-- peaks: [6, 3]} (or equivalent in other languages)
--
-- All input arrays will be valid integer arrays (although it could still be empty),
-- so you won't need to validate the input.
--
-- The first and last elements of the array will not be considered as peaks (in the context
-- of a mathematical function, we don't know what is after and before and therefore,
-- we don't know if it is a peak or not).
--
-- Also, beware of plateaus !!! [1, 2, 2, 2, 1] has a peak while [1, 2, 2, 2, 3] does not.
-- In case of a plateau-peak, please only return the position and value of the beginning
-- of the plateau. For example: pickPeaks([1, 2, 2, 2, 1]) returns {pos: [1], peaks: [2]}.


module PickPeak where

data PickedPeaks = PickedPeaks {pos :: [Int], peaks :: [Int]} deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks = pick Nothing 0 id

pick :: Maybe Int -> Int -> (Int -> Int) -> [Int] -> PickedPeaks
pick Nothing n _ (x:xs) = pick (Just x) (succ n) id xs
pick (Just prev) n p (x:next:xs)
  | prev >= x || next >= x = PickedPeaks pos peaks
  | otherwise = PickedPeaks (p n : pos) (x : peaks)
  where PickedPeaks pos peaks = if x == next
        then pick (Just prev) (succ n) (p . pred) (x:xs)
        else pick (Just x) (succ n) id (next:xs)
pick _ _ _ _ = PickedPeaks [] []
