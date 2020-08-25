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

import Data.Char (toLower)
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
         | Eq Sql Sql
         | Ne Sql Sql
         | Gt Sql Sql
         | Ge Sql Sql
         | Lt Sql Sql
         | Le Sql Sql
         | Or Sql Sql
         | And Sql Sql
         | Number Int
         | Quoted String
         | Column String String -- `tb-name` `col-name`
         deriving (Show, Read)


sqlEngine :: Database -> String -> [Dbo]
sqlEngine database = execute
  where
    execute :: String -> [Dbo]
    execute query = undefined

-- fixme `words` like 'Daniel Craig'
parse :: String -> Sql
parse = parsing . words . unpack . strip . pack . fst . seps . map toLower
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
  where make op = op (parset [a]) (parset [b])
parset [",",v] = parset [v]
parset [v]
  | isJust (readMaybe v :: Maybe Int) = Number $ read v
  | elem '.' v = uncurry Column $ fmap tail $ split '.' v
  | (&&) <$> (u . head) <*> (u . last) $ v = Quoted . tail . init $ v
  where u = (== '\'')


split :: Eq a => a -> [a] -> ([a], [a])
split t str = (takeWhile (/= t) str, dropWhile (/= t) str)
group :: Eq a => a -> a -> [[a]] -> [[a]]
group k t (s:ss) = (if t == k then ([]:) else ([]++)) $ (t : s) : ss

-- isQuoted :: String -> Bool
-- isQuoted = elem '.'
-- isNumber :: String -> Bool
-- isNumber = isJust . readMaybe :: (String -> Maybe Int)
-- isColumn :: String -> Bool
-- isColumn = (&&) <$> (u . head) <*> (u . last)
