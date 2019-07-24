module Main where

import Calculator

main :: IO ()
main = putStrLn . show $ evaluate "4 + 3 * 4 / 3 - 6 / 3 * 3 + 8"