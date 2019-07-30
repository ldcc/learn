module Main where

--import SimpleInteractiveInterpreter
--
--main :: IO ()
--main = putStrLn . show $ pass1 "fn avg x y => (x + y) / 2"



import TinyThreePassCompiler

main :: IO ()
main = putStrLn . show $ simulate (compile "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)") [4, 0, 0]