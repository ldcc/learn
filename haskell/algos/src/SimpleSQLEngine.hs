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
         | Quoted String
         | Column String String -- `tb-name`.`col-name`
         deriving (Show, Read)

data Cmp = Eq | Ne | Gt | Ge | Lt | Le deriving (Show, Read)
pickcmp :: Ord a => Cmp -> a -> a -> Bool
pickcmp Eq = (==)
pickcmp Ne = (/=)
pickcmp Gt = (>)
pickcmp Ge = (>=)
pickcmp Lt = (<)
pickcmp Le = (<=)

------------------------------------- interpreting -------------------------------------

sqlEngine :: Database -> String -> [Dbo]
sqlEngine db = indirect . flip pass db . parse where
  indirect :: Database -> [Dbo]
  indirect [] = []
  indirect [(_,dbos)] = dbos

pass :: Sql -> Database -> Database
pass _ [] = []
pass (Void) db = db
pass (Query s f w) db = pass s . pass w . pass f $ db
pass (Select cols) db = db >>= return . fmap ((map.filter) f) where
  f = (`elem` map comcol cols) . fst
pass (From tb joins) db = case lookup tb db of
  Nothing -> []
  Just dbos -> foldl f [addpre (tb, dbos)] joins where
    addpre :: Table -> Table
    addpre tbl = fmap (map.map $ \(k, v) -> (fst tbl++"."++k, v)) tbl
    f [acc] (Join jtb test) = case lookup jtb db of
      Nothing -> []
      Just jdbos -> pass test [(cartprod . addpre) (jtb, jdbos) acc]
pass (Where test) db = pass test db
pass (Test _cmp e1 e2) db@[(_, dbos)] = eval e1 e2
  where
    cmp = pickcmp _cmp
    eval :: Sql -> Sql -> Database
    eval (Quoted s1) (Quoted s2) = if cmp s1 s2 then db else []
    eval q@(Quoted _) c@(Column _ _) = eval c q
    eval c@(Column _ _) (Quoted s) = [fmap (filter f) $ head db] where
      f dbo = case lookup (comcol c) dbo of
        Nothing -> False
        Just v -> cmp s v
    eval c1@(Column _ _) c2@(Column _ _) = [fmap (filter f) $ head db] where
      f dbo = case lookup (comcol c1) dbo of
        Nothing -> False
        Just v1 -> case lookup (comcol c2) dbo of
          Nothing -> False
          Just v2 -> cmp v1 v2


------------------------------------- parsing -------------------------------------

parse :: String -> Sql
parse = parsing . wordsq . unpack . strip . pack . fst . seps
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
  | t == "=" = make Eq
  | t == ">" = make Gt
  | t == "<" = make Lt
  | t == ">=" = make Ge
  | t == "<=" = make Le
  | t == "<>" = make Ne
  where make op = Test op (parset [a]) (parset [b])
parset [",",v] = parset [v]
parset [v]
  | isJust (readMaybe v :: Maybe Int) = Quoted v
  | elem '.' v = uncurry Column $ fmap tail $ split '.' v
  | (&&) <$> (u . head) <*> (u . last) $ v = Quoted . tail . init $ v
  where u = (== '\'')

------------------------------------- misc -------------------------------------

cartprod :: Table -> Table -> Table
cartprod (tb, dbos1) (_, dbos2) = (tb, [dbo1 ++ dbo2 | dbo1 <- dbos1, dbo2 <- dbos2])

comcol :: Sql -> String
comcol (Column tb col) = tb ++ "." ++ col -- TODO case insensitive

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
      0 -> w' : nwords
      _ -> (w' ++ " " ++ head nwords) : (tail nwords)
      where
        (w, str'') = break isSpace str'
        w' = case map toLower w of
          "select" -> "select"
          "where" -> "where"
          "from" -> "from"
          "join" -> "join"
          _ -> w
        newn = case (head w, last w) of
          ('\'', '\'') -> n
          ('\'', _) -> n + 1
          (_, '\'') -> n - 1
          _ -> n
        nwords = wordsq' newn str''
