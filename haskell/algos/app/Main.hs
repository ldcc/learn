module Main where

import UnlimitedGameOfLife

glider0 = [[1,2,3],
           [4,5,6],
           [7,8,9]]

matrix = [[1,1,1],
          [1,1,1],
          [1,1,1]]

glider = [[1,0,0],
          [0,1,1],
          [1,1,0]]

gliders = [[[1,0,0],
            [0,1,1],
            [1,1,0]],
           [[0,1,0],
            [0,0,1],
            [1,1,1]],
           [[1,0,1],
            [0,1,1],
            [0,1,0]],
           [[0,0,1],
            [1,0,1],
            [0,1,1]]
          ]


main :: IO ()
main = do
  putStrLn . show $ getGeneration glider 0
  putStrLn . show $ getGeneration glider 1
  putStrLn . show $ getGeneration glider 2
  putStrLn . show $ getGeneration glider 3

[[0,1,0],[0,0,1],[1,1,1]]
