-- The marketing team is spending way too much time typing in hashtags.
-- Let's help them with our own Hashtag Generator!
--
-- Here's the deal:
--
-- It must start with a hashtag (#).
-- All words must have their first letter capitalized.
-- If the final result is longer than 140 chars it must return Nothing.
-- If the input or the result is an empty string it must return Nothing.

module Hashtag where

import Data.Char (isAlpha, isSpace, isSeparator, toUpper)

generateHashtag :: String -> Maybe String
generateHashtag str = if length str > 140 then Nothing else generate $ trim str
  where generate str = str >>= return . concatMap (\ (x:xs) -> toUpper x:xs) . words >>= return . (\s -> '#':s)

trim :: String -> Maybe String
trim = p . t . t . filter (\x -> or $ map ($ x) [isAlpha, isSpace]) where
  t = reverse . dropWhile isSpace
  p = (\x -> if x == [] then Nothing else Just x)
