module LastDigit where

lastDigit :: [Integer] -> Integer
lastDigit as = findDigit (reverse as) 1 `mod` 10

findDigit :: [Integer] -> Integer -> Integer
findDigit [] n = n
findDigit (a:as) n = findDigit as $ a ^ if n < 4 then n else n `mod` 4 + 4