module Main where

import Data.Either (fromRight)
import SimpleInteractiveInterpreter

main :: IO ()
main = putStrLn . show $ input "y = 1" newInterpreter >>= return . fst
