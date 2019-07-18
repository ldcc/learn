-- There is a secret string which is unknown to you. Given a collection of random
-- triplets from the string, recover the original string.
--
-- A triplet here is defined as a sequence of three letters such that each letter
-- occurs somewhere before the next in the given string. "whi" is a triplet for
-- the string "whatisup".
--
-- As a simplification, you may assume that no letter occurs more than once in
-- the secret string.
--
-- You can assume nothing about the triplets given to you other than that they
-- are valid triplets and that they contain sufficient information to deduce
-- the original string. In particular, this means that the secret string will
-- never contain letters that do not occur in one of the triplets given to you.


module RecoverSecretFromTriplets where

import Data.List (delete, elemIndex)

recoverSecret :: [String] -> String
recoverSecret ts = foldl1 (mh False) $ foldl (\ acc x -> foldl (mh True) x (delete x ts) : acc) [] ts

mh :: Bool -> String -> String -> String
mh _ [] _ = []
mh p (x:xs) t = case flip splitAt t <$> (+1) <$> elemIndex x t of
  Nothing -> x : if p then xs else mh p (delete x xs) t
  Just (l, []) -> l ++ xs
  Just (l, r) -> l ++ if xs == [] then r else mh p xs r
