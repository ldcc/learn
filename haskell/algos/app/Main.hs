module Main where

import Data.Either (fromRight)
import SimpleInteractiveInterpreter

main :: IO ()
main = putStrLn . show $ input "fn a => 3 + 2" newInterpreter >>= return . fst
