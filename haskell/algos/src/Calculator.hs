module Calculator where

data Ast = Proc Arith Ast Ast | V Double deriving (Show, Read)
data Arith = Plus | Minus | Times | Divide deriving (Show, Read)

evaluate :: String -> Double
evaluate = calc . fst . pass . parse . reverse . words

calc :: Ast -> Double
calc (Proc op l r) = (pickop op) (calc l) (calc r)
calc (V v) = v

pass :: [String] -> (Ast, [String])
pass (p:ps0)
  | p == "+" = (Proc Plus car cdr, ps2)
  | p == "-" = (Proc Minus car cdr, ps2)
  | p == "*" = (Proc Times car cdr, ps2)
  | p == "/" = (Proc Divide car cdr, ps2)
  | otherwise = (V (read p), ps0)
  where
    (car, ps1) = pass ps0
    (cdr, ps2) = pass ps1

parse :: [String] -> [String]
parse = parsing [] []
  where
  parsing :: [String] -> [String] -> [String] -> [String]
  parsing stack1 stack2 [] = merging stack1 stack2 [] (const True)
  parsing stack1 stack2 (t:ts)
    | elem t ["*", "/"] = parsing (t:stack1) stack2 ts
    | elem t ["+", "-"] = merging stack1 stack2 (t:ts) $ flip elem ["*", "/"]
    | otherwise = parsing stack1 (t:stack2) ts
  merging :: [String] -> [String] -> [String] -> (String -> Bool) -> [String]
  merging [] stack2 [] _ = stack2
  merging [] stack2 (t:ts) _ = parsing [t] stack2 ts
  merging (s:ss) stack2 tokens@(t:ts) p
    | p s = merging ss (s : stack2) tokens p
    | otherwise = parsing (t : s : ss) stack2 ts

pickop Plus = (+)
pickop Minus = (-)
pickop Times = (*)
pickop Divide = (/)