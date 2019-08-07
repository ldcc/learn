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
input prog env = parse prog >>= genAst env >>= interp env

parse :: String -> Either String [String]
parse = return . parsing [] [] . reverse . words . foldl (\ acc t -> acc ++ if elem t "+-*/%()" then [' ', t, ' '] else [t]) []
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
  gen ("=":k:ts) = gen ts >>= return . fmap (Assign k)
  gen (t:ts)
    | isJust $ (readMaybe t :: Maybe Double) = return $ fmap (Const . read) (ts, t)
    | member t $ snd env = genInvoke t ts
    | otherwise = return $ fmap Symbol (ts, t)
  gen _ = pmm_err
  genClosure k (args, (_:ts)) = gen ts >>= return . fmap (Closure k args)
  genInvoke k ts = let Closure _ args _ = snd env ! k in
    foldl f (return (ts, [])) args >>= return . fmap (Invoke k)
    where f acc _ = acc >>= \ (ts, exps) -> gen ts >>= return . fmap ((exps ++) . (:[]))

interp :: Interpreter -> Ast -> Either String (Result, Interpreter)
interp env (Const v) = return (return v, env)
interp env (Symbol k) = return ukno_err >>= fst env !? k >>=! interp env
--interp env (Assign k ast) = snd env !? k >>! interp env ast >>= return . fmap (fmap (insert k ast))
--interp env (Invoke k asts) = return (return n, env)
--interp env (Closure k args exp) = fst env ?*? k >> return (Nothing, insert k (Closure k args exp) env)

--class Monad (m) => EitherT (m) where
--  (>>!) :: m a -> (Either String a) -> (Either String a)
--
--instance EitherT (Maybe) where
--  (Just x) >>! y = y >> return x
--  Nothing >>! y = y >> Left "Transformation Error!"

class Monad m => MaybeT m where
  (>>!) :: Maybe a -> m b -> m b -> m b
  (>>=!) :: Maybe a -> (a -> m b) -> m b -> m b

instance MaybeT (Either e) where
  (Just _) >>! y = \ _ -> y
  Nothing >>! _ = \ z -> z
  (Just x) >>=! f = \ _ -> f x
  Nothing >>=! f = \ y -> y

--type Φ = Maybe
--type Ψ = Either e
--Γ :: Φ a -> Ψ a -> Ψ a
--Γ (Just a) = \ (Right x) -> Right
--(Ψ f) <*> Γ :: Φ a -> Ψ b
