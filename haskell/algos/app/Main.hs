module Main where

import Data.Either (fromRight)
import SimpleInteractiveInterpreter

main :: IO ()
main = putStrLn . show $ input "fn avg x y => (x + y) / 2" newInterpreter
