module NumberOfZeros where

zeros :: Int -> Int
zeros n = sum $ map (n `div`) $ takeWhile (< n) $ map (5 ^) [1..]
