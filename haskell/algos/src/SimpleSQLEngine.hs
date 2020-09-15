-- query         =  select, ws, from, [ ws, join ], [ ws, where ] ;
-- select        =  "SELECT ", column-id, [ { ", ", column-id } ] ;
-- from          =  "FROM ", table-name, [ { ws, join } ] ;
-- join          =  "JOIN ", table-name, " on ", value-test ;
-- where         =  "WHERE ", value-test ;
-- value-test    =  value, comparison, value;

-- table-name    = ? a valid Sql table name ? ;
-- column-name   = ? a valid Sql column name ? ;

-- column-id     =  table-name, ".", column-name ;
-- value         =  column-id | ? a number ? | ? a Sql single-quoted string ? ;
-- comparison    =  " = " | " > " | " < " | " <= " | " >= " | " <> " ;
-- ws            = " " | "\n" | ws, ws ;

module SimpleSQLEngine where

import Data.Char (toLower, isSpace)
import Data.Text (pack, unpack, strip)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

type Database = [Table]
type Table = (String, [Dbo])
type Dbo = [(String, String)]

data Sql = Query Sql Sql Sql    -- Select From Where
         | Select [Sql]         -- Select [Column]
         | From String [Sql]    -- From `tb-name` [Join]
         | Join String Sql      -- Join `tb-name` Test
         | Where Sql            -- Where Test
         | Void
         | Test Cmp Sql Sql
         | Number String
         | Quoted String
         | Column String String -- `tb-name`.`col-name`


type Cmp = String -> String -> Bool
-- data Cmp = Eq | Ne | Gt | Ge | Lt | Le deriving (Show, Read)


sqlEngine :: Database -> String -> [Dbo]
sqlEngine db0 = execute . flip pass db0 . parse
  where
    cartprod :: Table -> Table -> Table
    cartprod (tb1, dbos1) (tb2, dbos2) = ("ok", [dbo1 ++ dbo2 | dbo1 <- dbos1, dbo2 <- dbos2])
    execute :: Database -> [Dbo]
    execute [] = []
    execute [(_,dbos)] = dbos
    execute (tb1:tb2:db) = execute ((cartprod tb1 tb2) : db)

pass :: Sql -> Database -> Database
pass (Void) _ = []
pass (Query s f w) db = pass s . pass w . pass f $ db
pass (Select cols) db = db -- FIXME undefined
pass (From tb joins) db = case lookup tb db of
  Just dbos -> foldl f [(tb, dbos)] joins where
    f acc (Join jtb test) = case lookup jtb db of
      Just jdbos -> pass test $ (jtb, jdbos) : acc
      Nothing -> []
  Nothing -> []
pass (Where test) db = undefined
pass (Test cmp e1 e2) db = eval e1 e2
  where
    filtrate col v1 = filter (\dbo -> case lookup col dbo of
      Just v2 -> v1 == v2
      Nothing -> False)
    eval :: Sql -> Sql -> Database
    eval (Number i1) (Number i2) = if cmp i1 i2 then db else []
    eval (Quoted s1) (Quoted s2) = if cmp s1 s2 then db else []
    eval i@(Number _) c@(Column _ _) = eval c i
    eval (Column tb col) (Number i) = case pick tb db of
      Just (dbos, db1) -> case filtrate col i dbos of
        [] -> []
        tl -> (tb, tl) : db1
      Nothing -> []
    eval q@(Quoted _) c@(Column _ _) = eval c q
    eval (Column tb col) (Quoted s) = case lookup tb db of
      Just dbos -> undefined
      Nothing -> []
    eval (Column tb1 col1) (Column tb2 col2) = case lookup tb1 db of
      Just dbos1 -> case lookup tb2 db of
        Just dbos2 -> undefined
        Nothing -> []
      Nothing -> []

parse :: String -> Sql
parse = parsing . wordsq . unpack . strip . pack . fst . seps . map toLower
  where
    sep = \x -> (>>= \f -> if f x then (' ':[x], not . f) else ([x], f))
    seps = foldl (flip sep) ([], flip elem ",=<>")

parsing :: [String] -> Sql
parsing sql0 = Query (match select) (match from) (match pred)
  where
    (sql1, pred) = split "where" sql0
    (select, from) = split "from" sql1

match :: [String] -> Sql
match ("select":sql) = Select $ map parset $ foldr (group ",") [[]] sql
match ("from":tb:sql) = From tb $ map match . tail $ foldr (group "join") [[]] sql
match ("join":tb:_:sql) = Join tb $ parset sql
match ("where":sql) = Where $ parset sql
match _ = Void

parset :: [String] -> Sql
parset [a,t,b]
  | t == "=" = make (==)
  | t == "<>" = make (/=)
  | t == ">" = make (>)
  | t == "<" = make (<)
  | t == ">=" = make (>=)
  | t == "<=" = make (<=)
  where make op = Test op (parset [a]) (parset [b])
parset [",",v] = parset [v]
parset [v]
  | isJust (readMaybe v :: Maybe Int) = Number v
  | elem '.' v = uncurry Column $ fmap tail $ split '.' v
  | (&&) <$> (u . head) <*> (u . last) $ v = Quoted . tail . init $ v
  where u = (== '\'')


split :: Eq a => a -> [a] -> ([a], [a])
split = break . (==)
group :: Eq a => a -> a -> [[a]] -> [[a]]
group k t (s:ss) = (if t == k then ([]:) else ([]++)) $ (t : s) : ss
pick :: Eq a => a -> [(a, b)] -> Maybe (b, [(a, b)])
pick _ [] = Nothing
pick k (xy@(x,y) : xys)
  | k == x = Just (y, xys)
  | otherwise = fmap (xy:) <$> pick k xys
wordsq :: String -> [String]
wordsq = wordsq' 0
wordsq' n str = case dropWhile isSpace str of
    "" -> []
    str' -> case newn of
      0 -> w : nwords
      _ -> (w ++ " " ++ head nwords) : (tail nwords)
      where
        (w, str'') = break isSpace str'
        newn = case (head w, last w) of
          ('\'', '\'') -> n
          ('\'', _) -> n + 1
          (_, '\'') -> n - 1
          _ -> n
        nwords = wordsq' newn str''