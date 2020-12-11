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
    split (']':ts) = (words $ separates ts, [])
    split (t:ts) = let (nts, nt) = split ts in (nts, t:nt)
    separates = foldl (\ acc c -> acc ++ if elem c "+-*/()" then [' ', c, ' '] else [c]) []
    parsing stack1 stack2 [] = merging stack1 stack2 [] $ const True
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

pass2 :: AST -> AST
pass2 (Imm v) = Imm v
pass2 (Arg i) = Arg i
pass2 ast = recLR ast pass2 fst

pass3 :: AST -> [String]
pass3 (Imm v) = ["IM " ++ show v]
pass3 (Arg i) = ["AR " ++ show i]
pass3 ast = recLR ast pass3 $ \ op l r -> l ++ ["PU"] ++ r ++ ["SW", "PO", snd op]

recLR :: AST -> (AST -> a) -> ((AST -> AST -> AST, String) -> a -> a -> b) -> b
recLR (Add l r) f g = g (mk (+) Add, "AD") (f l) (f r)
recLR (Sub l r) f g = g (mk (-) Sub, "SU") (f l) (f r)
recLR (Mul l r) f g = g (mk (*) Mul, "MU") (f l) (f r)
recLR (Div l r) f g = g (mk div Div, "DI") (f l) (f r)

mk :: (Int -> Int -> Int) -> (AST -> AST -> AST) -> AST -> AST -> AST
mk f _ (Imm v1) (Imm v2) = Imm $ f v1 v2
mk _ g l r = g l r

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