module Main where

import BitCounting

main :: IO ()
main = do
  putStrLn . show $ countBits 9
  putStrLn . show $ countBits 7
