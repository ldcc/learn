module TinyThreePassCompiler where

import Text.Read (readMaybe)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 prog = fst . generates $ parsing [] [] $ reverse tokens
  where
    (tokens, params) = fmap words $ split prog
    split ('[':ts) = split ts
    split (']':ts) = (words $ seperates ts, [])
    split (t:ts) = let (nts, nt) = split ts in (nts, t:nt)
    seperates = foldl (\ acc t -> acc ++ if elem t "+-*/()" then [' ', t, ' '] else [t]) []
    generates (t:ts0)
      | elem t ["+", "-", "*", "/"] = let
          (car, ts1) = generates ts0
          (cdr, ts2) = generates ts1
        in case t of
          "+" -> (Add car cdr, ts2)
          "-" -> (Sub car cdr, ts2)
          "*" -> (Mul car cdr, ts2)
          "/" -> (Div car cdr, ts2)
      | isJust $ (readMaybe t :: Maybe Int) = (Imm $ read t, ts0)
      | otherwise = (Arg $ fromJust $ flip elemIndex params t, ts0)
    parsing stack1 stack2 [] = merging stack1 stack2 [] $ \_ -> True
    parsing stack1 stack2 (t:ts)
      | t == ")" = parsing (t:stack1) stack2 ts
      | t == "(" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | elem t ["*", "/"] = parsing (t:stack1) stack2 ts
      | elem t ["+", "-"] = merging stack1 stack2 (t:ts) $ flip elem ["*", "/"]
      | otherwise = parsing stack1 (t:stack2) ts
    merging [] stack2 [] _ = stack2
    merging [] stack2 (t:ts) _ = parsing [t] stack2 ts
    merging (s:ss) stack2 tokens p
      | p s = merging ss (s : stack2) tokens p
      | head tokens == "(" = parsing ss stack2 (tail tokens)
      | otherwise = parsing (head tokens : s : ss) stack2 (tail tokens)

pass2 :: AST -> AST
pass2 (Imm v) = Imm v
pass2 (Arg i) = Arg i
pass2 ast = case recLR ast pass2 id of
  Add (Imm v1) (Imm v2) -> Imm $ v1 + v2
  Sub (Imm v1) (Imm v2) -> Imm $ v1 - v2
  Mul (Imm v1) (Imm v2) -> Imm $ v1 * v2
  Div (Imm v1) (Imm v2) -> Imm $ v1 `div` v2
  nast -> nast

pass3 :: AST -> [String]
pass3 (Imm v) = ["IM " ++ show v]
pass3 (Arg i) = ["AR " ++ show i]
pass3 ast = let (_, l, r) = recLR ast pass3 (,,) in l ++ ["PU"] ++ r ++ ["SW", "PO", pickop ast]
  where
    pickop (Add _ _) = "AD"
    pickop (Sub _ _) = "SU"
    pickop (Mul _ _) = "MU"
    pickop (Div _ _) = "DI"

recLR :: AST -> (AST -> a) -> ((AST -> AST -> AST) -> a -> a -> b)  -> b
recLR (Add l r) f g = g Add (f l) (f r)
recLR (Sub l r) f g = g Sub (f l) (f r)
recLR (Mul l r) f g = g Mul (f l) (f r)
recLR (Div l r) f g = g Div (f l) (f r)

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0