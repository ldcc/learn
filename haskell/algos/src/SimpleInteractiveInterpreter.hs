module SimpleInteractiveInterpreter where

import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import Data.Map as Map (Map, fromList, member, insert, delete, (!), (!?))

type Result = Maybe Double
type Interpreter = Map String Ast
data Ast = Const Double
         | Symbol String
         | Assign String Ast
         | Invoke String [Ast]
         | Closure String [String] Ast deriving (Show, Read)

pmm_err = Left "Parameter Missmatch!"
conf_err = Left "Conflict Definition!"
ukno_err = Left "Unknown Identifier!"

newInterpreter :: Interpreter
newInterpreter = fromList $ "+-*/%" >>= \x -> [id &&& gen $ [x]]
  where gen s = Closure s ["x", "y"] $ Invoke s [Symbol "x", Symbol "y"]

--input :: String -> Interpreter -> Either String (Result, Interpreter)
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
genAst env ts = gen env ts >>= \ (nts, ast) -> if length nts > 0 then pmm_err else return ast
  where
  gen env ("fn":id:ts) = genClosure env id $ break (== "=>") ts
  gen env ("=":id:ts) = gen env ts >>= return . fmap (Assign id)
  gen env (t:ts)
    | member t env = genInvoke env t ts
    | isJust $ (readMaybe t :: Maybe Double) = return $ fmap (Const . read) (ts, t
    | otherwise = return $ fmap Symbol (ts, t)
  gen env _ = pmm_err
  genClosure env id (args, (_:ts)) = gen env ts >>= return . fmap (Closure id args)
  genInvoke env name ts = let Closure _ args _ = env ! name in
    foldl f (return (ts, [])) args >>= return . fmap (Invoke name)
    where f acc _ = acc >>= \ (ts, exps) -> gen env ts >>= return . fmap ((exps ++) . (:[]))

--interp :: Interpreter -> Ast -> Either String (Result, Interpreter)
interp env (Const n) = return (return n, env)
interp env (Symbol s) = lookup env s >>= interp env
interp env (Assign n) = return (return n, env)
interp env (Invoke n) = return (return n, env)
interp env (Closure n) = return (return n, env)

lookup :: Interpreter -> String -> Either String Ast
lookup env s = case env !? s of
  Just ast -> return ast
  Nothing -> ukno_err

--extend :: String -> Ast -> Interpreter -> Interpreter
--extend = insert
--
--extend :: String -> Interpreter -> Interpreter
--strikeOut = delete