module Main where

import TinyThreePassCompiler

main :: IO ()
main = putStrLn . show $ pass1 "[ a b ] a*a + b*b"