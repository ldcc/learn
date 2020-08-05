module Calculator where

data Ast = Proc Arith Ast Ast | V Double deriving (Show, Read)
data Arith = Plus | Minus | Times | Divide deriving (Show, Read)

evaluate :: String -> Double
evaluate = calc . fst . pass . parse . reverse . words

pass :: [String] -> (Ast, [String])
pass [p] = (V (read p), [])
pass (p:ps0)
  | isp p = (Proc Plus car cdr, ps2)
  | iss p = (Proc Minus car cdr, ps2)
  | ism p = (Proc Times car cdr, ps2)
  | isd p = (Proc Divide car cdr, ps2)
  | otherwise = (V (read p), ps0)
  where
    (car, ps1) = pass ps0
    (cdr, ps2) = pass ps1

calc :: Ast -> Double
calc (Proc op l r) = (pickop op) (calc l) (calc r)
calc (V v) = v

parse :: [String] -> [String]
parse = parsing [] []
  where
  parsing :: [String] -> [String] -> [String] -> [String]
  parsing stack1 stack2 [] = merging stack1 stack2 [] (\_ -> True)
  parsing stack1 stack2 (t:ts)
    | any ($ t) [ism, isd] = parsing (t:stack1) stack2 ts
    | any ($ t) [isp, iss] = merging stack1 stack2 (t:ts) id
    | otherwise = parsing stack1 (t:stack2) ts
  merging :: [String] -> [String] -> [String] -> (Bool -> Bool) -> [String]
  merging [] stack2 [] _ = stack2
  merging [] stack2 (t:ts) _ = parsing [t] stack2 ts
  merging (s:ss) stack2 tokens p
    | any (p . ($ s)) [ism, isd] = merging ss (s : stack2) tokens p
    | otherwise = parsing (head tokens : s : ss) stack2 (tail tokens)

isp = (== "+")
iss = (== "-")
ism = (== "*")
isd = (== "/")

pickop Plus = (+)
pickop Minus = (-)
pickop Times = (*)
pickop Divide = (/)