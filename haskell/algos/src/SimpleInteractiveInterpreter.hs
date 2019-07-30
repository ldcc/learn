module SimpleInteractiveInterpreter where

import Text.Read (readMaybe)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

--- Interpreter which represented the REPL environment
type Result = Maybe Double
data Interpreter
data Arith = Plus | Minus | Times | Divide deriving (Show, Read)
data Ast = Const Double
         | Symbol String
         | Proc Arith Ast Ast
         | Invoke String [Ast]
         | Closure String [Ast] Ast deriving (Show, Read)

newInterpreter :: Interpreter
newInterpreter = undefined

input :: String -> Interpreter -> Either String (Result, Interpreter)
input _ _ = Left "Not implemented"

pass1 :: String -> [String]
pass1 = reverse . words . foldl (\ acc t -> acc ++ if elem t "+-*/()" then [' ', t, ' '] else [t]) []
  where
--    generates (t:ts0)
--      | elem t ["+", "-", "*", "/"] = let
--          (car, ts1) = generates ts0
--          (cdr, ts2) = generates ts1
--        in case t of
--          "+" -> (Add car cdr, ts2)
--          "-" -> (Sub car cdr, ts2)
--          "*" -> (Mul car cdr, ts2)
--          "/" -> (Div car cdr, ts2)
--      | isJust $ (readMaybe t :: Maybe Int) = (Imm $ read t, ts0)
--      | otherwise = (Arg $ fromJust $ flip elemIndex params t, ts0)
    parsing stack1 stack2 [] = merging stack1 stack2 [] $ \_ -> True
    parsing stack1 stack2 (t:ts)
      | t == ")" = parsing (t:stack1) stack2 ts
      | t == "(" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | t == "=" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | t == "=>" = merging stack1 stack2 (t:ts) $ \_ -> True
      | elem t ["*", "/"] = parsing (t:stack1) stack2 ts
      | elem t ["+", "-"] = merging stack1 stack2 (t:ts) $ flip elem ["*", "/"]
      | otherwise = parsing stack1 (t:stack2) ts
    merging [] stack2 [] _ = stack2
    merging [] stack2 (t:ts) _
      | t == "=>" = parsing [] (t:stack2) ts
      | otherwise = parsing [t] stack2 ts
    merging (s:ss) stack2 tokens p
      | p s = merging ss (s : stack2) tokens p
      | head tokens == "(" = parsing ss stack2 (tail tokens)
      | otherwise = parsing (head tokens : s : ss) stack2 (tail tokens)