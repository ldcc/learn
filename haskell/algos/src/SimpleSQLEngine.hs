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

data SQL = Query SQL String SQL SQL  -- [Column] From Join Where
         | Select [Value]
         | Join String [Vtest]
         | Where [Vtest]
         | Void
         deriving (Show, Read)
data Vtest = Vtest Compare Value Value deriving (Show, Read)
data Value = Number Int              -- readMaybe t :: Maybe Int
           | Quoted String
           | Column String String    -- TableName ColumnName
           deriving (Show, Read)
data Compare = Eq | Gt | Ge | Lt | Le | Ne deriving (Show, Read)


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

-- parse :: String -> SQL



-- isTest ::
-- isComparison :: String -> Bool
-- isColumn :: String -> Bool
-- isConst :: String -> Bool
-- isNumber :: String -> Bool -- readMaybe
-- isQuoted :: String -> Bool


