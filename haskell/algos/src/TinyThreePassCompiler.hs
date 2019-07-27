module TinyThreePassCompiler where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

compile :: String -> [String]
compile = pass3 . pass2 . pass1


pass1 :: String -> AST
pass1 prog = fst . generates $ parse $ reverse tokens
  where
    (params, tokens) = split $ tokenize prog
    generates :: [Token] -> (AST, [Token])
    generates (TInt v:ts) = (Imm v, ts)
    generates (TStr s:ts) = (Arg $ fromJust $ flip elemIndex params $ TStr s, ts)
    generates (TChar c:ts0) = case c of
      '+' -> (Add car cdr, ts2)
      '-' -> (Sub car cdr, ts2)
      '*' -> (Mul car cdr, ts2)
      '/' -> (Div car cdr, ts2)
      where
        (car, ts1) = generates ts0
        (cdr, ts2) = generates ts1

pass2 :: AST -> AST
pass2 (Imm v) = Imm v
pass2 (Arg i) = Arg i
pass2 ast = case getLR ast of
  Add (Imm v1) (Imm v2) -> Imm $ v1 + v2
  Sub (Imm v1) (Imm v2) -> Imm $ v1 - v2
  Mul (Imm v1) (Imm v2) -> Imm $ v1 * v2
  Div (Imm v1) (Imm v2) -> Imm $ v1 `div` v2
  nast -> nast
  where
    getLR :: AST -> AST
    getLR (Add l r) = Add (pass2 l) (pass2 r)
    getLR (Sub l r) = Sub (pass2 l) (pass2 r)
    getLR (Mul l r) = Mul (pass2 l) (pass2 r)
    getLR (Div l r) = Div (pass2 l) (pass2 r)

--pass3 :: AST -> [String]
pass3 (Imm v) = ["IM " ++ show v]
pass3 (Arg i) = ["AR " ++ show i]
pass3 ast = let (l, r) = getLR ast in l ++ ["PU"] ++ r ++ ["SW", "PO"] ++ [pickop ast]
  where
    getLR :: AST -> ([String], [String])
    getLR (Add l r) = (pass3 l, pass3 r)
    getLR (Sub l r) = (pass3 l, pass3 r)
    getLR (Mul l r) = (pass3 l, pass3 r)
    getLR (Div l r) = (pass3 l, pass3 r)
    pickop :: AST -> String
    pickop (Add _ _) = "AD"
    pickop (Sub _ _) = "SU"
    pickop (Mul _ _) = "MU"
    pickop (Div _ _) = "DI"


alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

split :: [Token] -> ([Token], [Token])
split (TChar '[':ts) = split ts
split (TChar ']':ts) = ([], ts)
split (t:ts) = let (nt, nts) = split ts in (t:nt, nts)

parse :: [Token] -> [Token]
parse = parsing [] []
  where
  parsing :: [Token] -> [Token] -> [Token] -> [Token]
  parsing stack1 stack2 [] = merging stack1 stack2 [] [tau]
  parsing stack1 stack2 (t:ts)
    | isr t = parsing (t:stack1) stack2 ts
    | isl t = merging stack1 stack2 (t:ts) [not . isr]
    | any ($ t) [ism, isd] = parsing (t:stack1) stack2 ts
    | any ($ t) [isp, iss] = merging stack1 stack2 (t:ts) [ism, isd]
    | otherwise = parsing stack1 (t:stack2) ts
  merging :: [Token] -> [Token] -> [Token] -> [(Token -> Bool)] -> [Token]
  merging [] stack2 [] _ = stack2
  merging [] stack2 (t:ts) _ = parsing [t] stack2 ts
  merging (s:ss) stack2 tokens ps
    | any ($ s) ps = merging ss (s : stack2) tokens ps
    | isl $ head tokens = parsing ss stack2 (tail tokens)
    | otherwise = parsing (head tokens : s : ss) stack2 (tail tokens)
  isp, iss, ism, isd, isl, isr, tau :: Token -> Bool
  isp = (== TChar '+')
  iss = (== TChar '-')
  ism = (== TChar '*')
  isd = (== TChar '/')
  isl = (== TChar '(')
  isr = (== TChar ')')
  tau _ = True

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