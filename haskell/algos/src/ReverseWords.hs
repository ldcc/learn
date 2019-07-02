module ReverseWords (reverseWords) where

import Data.List (intercalate)

reverseWords :: String -> String
reverseWords = intercalate " " . foldl sp []

sp :: [String] -> Char -> [String]
sp [] x = [[x]]
sp acc ' ' = [] : acc
sp (xs : acc) x = (xs ++ [x]) : acc