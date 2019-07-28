module Main where

import TinyThreePassCompiler

main :: IO ()
main = putStrLn . show $ simulate (compile "[ a b ] a*a + b*b") [1, 2]