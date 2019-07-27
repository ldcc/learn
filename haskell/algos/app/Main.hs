module Main where

import TinyThreePassCompiler

main :: IO ()
main = putStrLn . show $ simulate (compile "[ x ] x + 2 * 5") [3]