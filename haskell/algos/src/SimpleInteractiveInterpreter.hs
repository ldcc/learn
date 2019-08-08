module SimpleInteractiveInterpreter where

import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import Data.Map as Map (Map, fromList, member, insert, delete, (!), (!?))

type Result = Maybe Double
type Interpreter = (Map String Ast, Map String Ast)
data Ast = Const Double
         | Symbol String
         | Assign String Ast
         | Invoke String [Ast]
         | Closure String [String] Ast deriving (Show, Read)

pmm_err = Left "Parameter Missmatch!"
conf_err = Left "Conflict Definition!"
ukno_err = Left "Unknown Identifier!"

newInterpreter :: Interpreter
newInterpreter = (fromList [("x", Const 5)], fromList $ "+-*/%" >>= \x -> [id &&& gen $ [x]])
  where gen s = Closure s ["x", "y"] $ Invoke s [Symbol "x", Symbol "y"]

input :: String -> Interpreter -> Either String (Result, Interpreter)
input prog env = return prog >>= return . parse >>= genAst env >>= interp env

parse :: String -> [String]
parse = parsing [] [] . reverse . words . foldl (\ acc t -> acc ++ if elem t "+-*/%()" then [' ', t, ' '] else [t]) []
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

genAst :: Interpreter -> [String] -> Either String Ast
genAst env ts = gen ts >>= \ (nts, ast) -> if length nts > 0 then pmm_err else return ast
  where
  gen ("fn":k:ts) = genClosure k $ break (== "=>") ts
  gen ("=":k:ts) = fmap (Assign k) <$> gen ts
  gen (t:ts)
    | isJust $ (readMaybe t :: Maybe Double) = return $ fmap (Const . read) (ts, t)
    | member t $ snd env = genInvoke t ts
    | otherwise = return $ fmap Symbol (ts, t)
  gen _ = pmm_err
  genClosure k (args, (_:ts)) = fmap (Closure k args) <$> gen ts
  genInvoke k ts = let Closure _ args _ = snd env ! k in
    fmap (Invoke k) <$> foldl f (return (ts, [])) args
    where f acc _ = acc >>= \ (ts, exps) -> fmap ((exps ++) . return) <$> gen ts

interp :: Interpreter -> Ast -> Either String (Result, Interpreter)
interp env (Const v) = return (return v, env)
interp env (Symbol k) = fst env !? k !>>= interp env >>! ukno_err
interp env (Assign k ast) = snd env !? k !>> conf_err >>! (fmap (fmap (insert k ast)) <$> interp env ast)
--interp env (Invoke k asts) = fmap (interp env) <$> asts
--interp env (Closure k args exp) = fst env ?*? k >> return (Nothing, insert k (Closure k args exp) env)

class Monad m => MaybeT m where
  (!>>) :: Maybe a -> m b -> m b -> m b
  (!>>=) :: Maybe a -> (a -> m b) -> m b -> m b
  (>>!) :: (a -> m b) -> a -> m b

instance MaybeT (Either e) where
  Nothing !>> _ = id
  _ !>> y = (>> y)
  Nothing !>>= _ = id
  (Just x) !>>= f = \ _ -> f x
  f >>! x = f x

--instance MaybeT [] where
--  xs !>> y = \ _ -> y
--  xs !>>= f = \ _ -> f x
--  f >>! x = f x

--Three Unit Operator of Maybe Monad Transformer

-- maybe !>> e1 >>! e2 <=> case maybe of Just _ -> e1; Nothing -> e2
-- maybe !>>= f >>! e <=> case maybe of Just x -> f x; Nothing -> e
-- (f >>! x) <=> (f =<< return x)

--type Φ = Maybe
--type Ψ = Either e
--Γ :: Φ a -> Ψ a -> Ψ a
--Γ (Just a) = \ (Right x) -> Right
--(Ψ f) <*> Γ :: Φ a -> Ψ b
