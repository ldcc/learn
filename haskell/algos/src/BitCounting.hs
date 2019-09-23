module BitCounting (countBits) where

import Data.Char (intToDigit)

countBits :: Int -> Int
countBits = length . filter (== '1') . ctb

-- which mena `convert decimal to binary`
ctb :: Int -> String
ctb n = if n < 2 then show n else intToDigit (n `mod` 2) : ctb (n `div` 2)