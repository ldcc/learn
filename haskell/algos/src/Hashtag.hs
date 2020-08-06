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

import Data.Char (toUpper)

generateHashtag :: String -> Maybe String
generateHashtag s = let w = words s in
  if length s > 140 || w == [] then Nothing
  else Just . ('#':) $ concatMap (\ (x:xs) -> toUpper x:xs) w
