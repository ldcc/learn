module Main where

import UnlimitedGameOfLife

glider0 = [[1,2,3],
           [4,5,6],
           [7,8,9]]


glider = [[1,0,0],
          [0,1,1],
          [1,1,0]]


main :: IO ()
main = do
  putStrLn . show $ getGeneration glider 2
