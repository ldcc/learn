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

data Query = Select Ws From Ws Join Ws Where
data Select = Select Ws Columns
data Columns = Column Ws String | Column Ws String Ws String Ws Columns
data From = From Ws String | From Ws String Ws Join
data Join  = Join String VTest
data VTest = Eq Value Value
           | Gt Value Value
           | Ge Value Value
           | Lt Value Value
           | Le Value Value
           | Ne Value Value
data Value = Column String | Number String | Quoted String
data Ws = Empty | Space | Newline | Ws Ws deriving (Show, Read)

sqlEngine :: Database -> String -> [Dbo]
sqlEngine database = execute

execute :: String -> [Dbo]
execute query = undefined

-- isTest ::
-- isComparison :: String -> Bool
-- isColumn :: String -> Bool
-- isConst :: String -> Bool
-- isNumber :: String -> Bool -- readMaybe
-- isQuoted :: String -> Bool


