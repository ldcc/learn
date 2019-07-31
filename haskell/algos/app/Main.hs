module Main where

import SimpleInteractiveInterpreter

main :: IO ()
main = putStrLn . show $ input "fn avg x y => (x + y) / 2" newInterpreter -- (fn avg x y => e) ::= (avg = fn x y -> e)