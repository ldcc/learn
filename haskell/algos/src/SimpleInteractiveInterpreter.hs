module SimpleInteractiveInterpreter where

import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import Data.Map as Map (Map, fromList, lookup, insert, (!), member)

type Result = Maybe Double
type Interpreter = Map String Ast
data Ast = Const Double
         | Symbol String
         | Assign String Ast
         | Invoke String [Ast]
         | Closure [Ast] Ast deriving (Show, Read)

newInterpreter :: Interpreter
newInterpreter = fromList $ "+-*/%" >>= \x -> [id &&& gen $ [x]]
  where gen s = Closure [Symbol "x", Symbol "y"] $ Invoke s [Symbol "x", Symbol "y"]

--input :: String -> Interpreter -> Either String Ast
input prog interp = Right (parse prog) >>= genAst interp

parse :: String -> [String]
parse = parsing [] [] . reverse . words . foldl (\ acc t -> acc ++ if elem t "+-*/()" then [' ', t, ' '] else [t]) []
  where
    parsing stack1 stack2 [] = merging stack1 stack2 [] $ \_ -> True
    parsing stack1 stack2 (t:ts)
      | t == ")" = parsing (t:stack1) stack2 ts
      | t == "(" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | t == "=" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | t == "=>" = merging stack1 stack2 (t:ts) $ \_ -> True
      | elem t ["*", "/", "%"] = parsing (t:stack1) stack2 ts
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

--genAst :: Interpreter -> [String] -> Either String (Ast, [String], Interpreter)
genAst interp ("fn":name:ts) = case Map.lookup name interp of
  Just (Const _) -> (Left "Conflicts Error!", [])
  _ -> genClosure interp name $ break (== "=>") ts
genAst interp ("=":name:ts) = case Map.lookup name interp of
  Just (Closure _ _) -> (Left "Conflicts Error!", [], interp)
  _ -> genAst interp ts >>= \ (exp, nts, ninterp) -> Right (Assign name exp, nts, ninterp)
genAst interp (t:ts)
  | member t interp = Right $ genInvoke interp t ts
  | isJust $ (readMaybe t :: Maybe Double) = (Right $ Const $ read t, ts)
--  | otherwise = (Right $ Symbol t, ts)


--genClosure interp name (pre, (_:exp)) =


--  | member interp t =
--  | elem t ["+", "-", "*", "/"] = let
--      (car, ts1) = generates ts0
--      (cdr, ts2) = generates ts1
--    in case t of
--      "+" -> (Add car cdr, ts2)
--      "-" -> (Sub car cdr, ts2)
--      "*" -> (Mul car cdr, ts2)
--      "/" -> (Div car cdr, ts2)
--  | isJust $ (readMaybe t :: Maybe Int) = (Imm $ read t, ts0)
--  | otherwise = (Arg $ fromJust $ flip elemIndex params t, ts0)

--closure
