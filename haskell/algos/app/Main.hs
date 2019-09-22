module Main where

import Potatoes

main :: IO ()
main = do
  putStrLn . show $ potatoes 99 100 98
  putStrLn . show $ potatoes 84 65 80
  putStrLn . show $ potatoes 75 96 70
