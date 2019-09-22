module Main where

import LastDigit

main :: IO ()
main = do
  putStrLn . show $ lastDigit [1,2]
  putStrLn . show $ lastDigit [3,4,5]
  putStrLn . show $ lastDigit [7,6,21]
  putStrLn . show $ lastDigit [8,21]
  putStrLn . show $ lastDigit [2,2,2,0]
  putStrLn . show $ lastDigit [12,30,21]
  putStrLn . show $ lastDigit [123232,694022,140249]
  putStrLn . show $ lastDigit [499942,898102,846073]
