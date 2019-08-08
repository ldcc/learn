module Main where

import Data.Either (fromRight)
import SimpleInteractiveInterpreter

main :: IO ()
main = putStrLn . show $ input "x = 2" newInterpreter >>= return . fst
