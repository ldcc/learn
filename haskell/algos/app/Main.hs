module Main where

import MergeSort

main :: IO ()
main = do
  putStrLn . show $ merge [1] [0]