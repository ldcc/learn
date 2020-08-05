-- query         =  select, ws, from, [ ws, join ], [ ws, where ] ;
-- select        =  "SELECT ", column-id, [ { ", ", column-id } ] ;
-- from          =  "FROM ", table-name, [ { ws, join } ] ;
-- join          =  "JOIN ", table-name, " on ", value-test ;
-- where         =  "WHERE ", value-test ;
-- value-test    =  value, comparison, value;

-- table-name    = ? a valid SQL table name ? ;
-- column-name   = ? a valid SQL column name ? ;

-- column-id     =  table-name, ".", column-name ;
-- value         =  column-id | ? a number ? | ? a SQL single-quoted string ? ;
-- comparison    =  " = " | " > " | " < " | " <= " | " >= " | " <> " ;
-- ws            = " " | "\n" | ws, ws ;

module SimpleSQLEngine where

type Database = [Table]
type Table = (String, [Dbo])
type Dbo = [(String, String)]

type Query = ([String], String, Join, Where) -- ([column-id], table-name, Join, Where)
type Join = Maybe (String, [Vtest])            -- (table-name, value-test)
type Where = Maybe [Vtest]
type Vtest = Compare -> Value -> Value -> [Table]

-- vtest :: [Table] -> Compare -> Value -> Value -> [Table]

data Compare = Eq | Gt | Ge | Lt | Le | Ne deriving (Show, Read)
data Value = Number Int
           | Quoted String
           | Column String deriving (Show, Read) -- table-name, ".", column-name


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





-- isTest ::
-- isComparison :: String -> Bool
-- isColumn :: String -> Bool
-- isConst :: String -> Bool
-- isNumber :: String -> Bool -- readMaybe
-- isQuoted :: String -> Bool


