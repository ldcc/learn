module SimpleInteractiveInterpreter where

import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import Data.Map (Map, fromList, insert, (!), (!?))
import qualified Data.Set as Set (fromList, size)

type Result = Maybe Int
type Interpreter = (Map String Ast, Map String Ast)
data Ast = Void
         | Const Int
         | Symbol String
         | Assign String Ast
         | Invoke String [Ast]
         | Closure String [String] Ast
         deriving (Show, Read)

class Monad m => MonadT m where
  (>>?) :: Monad n => m a -> n b -> n b -> n b
  (>>=?) :: Monad n => m a -> (a -> n b) -> n b -> n b

instance MonadT Maybe where
  Nothing >>? _ = id
  _ >>? y = \ _ -> y
  Nothing >>=? _ = id
  (Just x) >>=? f = \ _ -> f x

arith_set = ["+","-","*","/","%"]
pmm_err = Left "Parameter Missmatch!"
ukno_err = Left "Unknown Identifier!"
conf_err = Left "Conflict Definition!"

newInterpreter :: Interpreter
newInterpreter = (fromList [], fromList $ arith_set >>= return . (id &&& gen))
  where gen s = Closure s ["a", "b"] Void

input :: String -> Interpreter -> Either String (Result, Interpreter)
input prog env = return (parse prog) >>= genAst env >>= checkAst >>= interp env

--------------------------- parser ---------------------------

parse :: String -> [String]
parse = parsing [] [] . reverse . words . foldl (\ acc t -> acc ++ if elem t "+-*/%()" then [' ', t, ' '] else [t]) []
  where
    parsing stack1 stack2 [] = merging stack1 stack2 [] $ const True
    parsing stack1 stack2 (t:ts)
      | t == ")" = parsing (t:stack1) stack2 ts
      | t == "(" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | t == "=" = merging stack1 stack2 (t:ts) $ not . (==) ")"
      | t == "=>" = merging stack1 stack2 (t:ts) $ const True
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
genAst env [] = return Void
genAst env ts = gen ts >>= \ (nts, ast) -> if length nts > 0 then pmm_err else return ast
  where
    gen [] = pmm_err
    gen ("=":k:ts) = fmap (Assign k) <$> gen ts
    gen ("fn":k:ts) = genClosure k $ break (== "=>") ts
    gen (t:ts)
      | isJust $ (readMaybe t :: Maybe Int) = return $ fmap (Const . read) (ts, t)
      | isJust $ snd env !? t = genInvoke t ts
      | otherwise = return $ fmap Symbol (ts, t)
    genClosure k (args, (_:ts)) = fmap (Closure k args) <$> gen ts
    genInvoke k ts = let Closure _ args _ = snd env ! k in fmap (Invoke k) <$> foldl f (return (ts, [])) args
    f acc _ = acc >>= \ (ts, asts) -> fmap ((asts <>) . return) <$> gen ts

checkAst :: Ast -> Either String Ast
checkAst (Closure k args exp)
   | checkExpr args exp && checkArgs args = return (Closure k args exp)
   | otherwise = ukno_err
  where
    checkExpr args (Symbol s) = elem s args
    checkExpr args (Invoke k asts) = all (checkExpr args) asts
    checkExpr _ _ = True
    checkArgs args = length args == (Set.size $ Set.fromList args)
checkAst ast = return ast

--------------------------- interp ---------------------------

interp :: Interpreter -> Ast -> Either String (Result, Interpreter)
interp env (Void) = return (Nothing, env)
interp env (Const v) = return (return v, env)
interp env (Symbol k) = fst env !? k >>=? interp env $ ukno_err
interp env (Assign k ast) = snd env !? k >>? conf_err $ fmap (fmapL (insert k ast)) <$> interp env ast
interp env (Invoke "+" asts) = calc (+) asts env
interp env (Invoke "-" asts) = calc (-) asts env
interp env (Invoke "*" asts) = calc (*) asts env
interp env (Invoke "/" asts) = calc div asts env
interp env (Invoke "%" asts) = calc rem asts env
interp env (Invoke k asts) = snd env !? k >>=? invoke $ ukno_err
  where invoke (Closure _ args exp) = interp (extEnv args asts env) exp
interp env (Closure k args exp) = fst env !? k >>? conf_err $ interp nenv Void
  where nenv = fmap (insert k (Closure k args exp)) env

calc :: (Int -> Int -> Int) -> [Ast] -> Interpreter -> Either String (Result, Interpreter)
calc f [l, r] env0 = do
  (x, env1) <- interp env0 l
  (y, env2) <- interp env1 r
  return (f <$> x <*> y, env2)

extEnv :: [String] -> [Ast] -> Interpreter -> Interpreter
extEnv (n:ns) (a:as) = extEnv ns as . fmapL (insert n a)
extEnv [] [] = id

fmapL :: (a -> b) -> ((,) a) t -> ((,) b) t
fmapL f (x, y) = (f x, y)

--class Monad m => MonadT m where
--  (-?) :: Monad n => m a -> n b -> n b -> n b
--  (\?) :: Monad n => m a -> (a -> n b) -> n b -> n b
--
--instance MonadT Maybe where
--  Nothing -? _ = id
--  _ -? y = \ _ -> y
--  Nothing \? _ = id
--  (Just x) \? f = \ _ -> f x
--
--(-:) :: Monad m => (a -> m b) -> a -> m b
--f -: x = return x >>= f

--type Φ = Maybe
--type Ψ = Either e
--Γ :: Φ a -> Ψ b -> Ψ b
--(Ψ f) <*> Γ :: Φ a -> Ψ b
