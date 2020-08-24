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
import qualified Data.Text as T (pack, unpack, strip)

type Database = [Table]
type Table = (String, [Dbo])
type Dbo = [(String, String)]

data Sql = Select [Value] Sql Sql
         | From String [Join]
         | Join String Vtest
         | Where Vtest
         | Void
         deriving (Show, Read)

data Vtest = Eq Vtest Vtest
           | Ne Vtest Vtest
           | Gt Vtest Vtest
           | Ge Vtest Vtest
           | Lt Vtest Vtest
           | Le Vtest Vtest
           | Or Vtest Vtest
           | And Vtest Vtest
           | Number Int              -- readMaybe t :: Maybe Int
           | Quoted String
           | Column String String    -- TableName ColumnName
           deriving (Show, Read)


sqlEngine :: Database -> String -> [Dbo]
sqlEngine database = execute
  where
    execute :: String -> [Dbo]
    execute query = undefined
--     lt :: Compare
--     le :: Compare
--     gt :: Compare
--     ge :: Compare
--     eq :: Compare
--     ne :: Compare

-- parse :: String -> Sql
parse = parsing . words . T.unpack . T.strip . T.pack . fst . seps . map C.toLower
  where
    sep = \x -> (>>= \f -> if f x then (' ':[x], not . f) else ([x], f))
    seps = foldl (flip sep) ([], flip elem syms)

parsing sql0 =
  where
    preds = dropWhile (/= "where") sql0
--   [sql1] ->
--   [sql1, pred] -> []
--   w@_ -> w
--
--   _ -> Select []

-- parsing :: Text -> Sql
-- parsing sql0 = sql0
--   where
--     [sql1, pred] = splitOn (pack "where") sql0
--     [sql2, ]

match :: String -> Sql
match preds@("select":_) = Where Vtest Eq Value Value


-- isTest ::
-- isComparison :: String -> Bool
-- isColumn :: String -> Bool
-- isConst :: String -> Bool
-- isNumber :: String -> Bool -- readMaybe
-- isQuoted :: String -> Bool

symbols = [",", "=", ">", "<", "<=", ">=", "<>"]
syms = ",=<>"
