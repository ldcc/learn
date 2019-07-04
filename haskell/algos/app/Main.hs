module Main where

import PickPeak

main :: IO ()
main = putStrLn . show $ pickPeaks [2,1,3,3,3,1,1,2,1]
