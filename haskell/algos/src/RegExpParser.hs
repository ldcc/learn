module RegExpParser where

--  TODO >>= -> <$> | <*>

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater -> `.`
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp -> `*`
            | Or RegExp RegExp  -- ^ A choice between 2 regexps -> `|`
            | Str [RegExp]      -- ^ A sequence of regexps.
            | None
            deriving (Show, Eq)

parseRegExp :: String -> Maybe RegExp
parseRegExp [] = Nothing
parseRegExp token = splitExp 0 token >>= \ (r, l) -> case r of
  [] -> parsing l
  _ -> Or <$> parseRegExp l <*> parseRegExp r

parsing :: String -> Maybe RegExp
parsing [] = Just None
parsing ['.'] = Just Any
parsing ['*'] = Just $ ZeroOrMore None
parsing [char] = Just $ Normal char
parsing (t:ts) = ret >>= \ (r, l) -> match <$> parseRegExp l <*> parsing r
  where ret | t == '(' = pickExp 0 (t:ts)
            | otherwise = Just (ts, [t])

match e1 None = e1
match e1 (Str exps) = Str $ e1 : exps
match e1 (ZeroOrMore None) = ZeroOrMore e1
match e1 e2 = Str [e1, e2]

splitExp :: Int -> String -> Maybe (String, String)
splitExp 0 ('|':ts) = Just (ts, [])
splitExp 0 [] = Just ([], [])
splitExp (-1) _ = Nothing
splitExp _ [] = Nothing
splitExp p (s:ts) = fmap (s:) <$> splitExp newp ts
  where newp | s == '(' = p+1
             | s == ')' = p-1
             | otherwise = p

pickExp :: Int -> String -> Maybe (String, String)
pickExp 0 ('(':ts) = pickExp 1 ts
pickExp 1 (')':ts) = Just (ts, [])
pickExp 0 [] = Just ([], [])
pickExp (-1) _ = Nothing
pickExp _ [] = Nothing
pickExp p (s:ts) = fmap (s:) <$> pickExp newp ts
  where newp | s == '(' = p+1
             | s == ')' = p-1
             | otherwise = p
