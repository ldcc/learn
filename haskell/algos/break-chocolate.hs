module BreakingChocolate where

breakChocolate :: Int -> Int -> Int
-- breakChocolate n m = 
--     let x = n - 1
--         y = m - 1
--     in  x + y * n
breakChocolate n m
    | n /= 0 && m /= 0 = n * m - 1
    | otherwise        = 0