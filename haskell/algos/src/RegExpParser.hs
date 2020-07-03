module RegExpParser where

import Data.Maybe (isJust, fromJust)

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater -> `.`
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp -> `*`
            | Or RegExp RegExp  -- ^ A choice between 2 regexps -> `|`
            | Str [RegExp]      -- ^ A sequence of regexps.
            deriving (Show, Eq)

parseRegExp :: String -> Maybe RegExp
parseRegExp [] = Nothing
parseRegExp token = case r of
  [] -> parsing l
  _ -> Just $ Or (fromJust $ parseRegExp l) (fromJust $ parseRegExp r)
  where (r, l) = splitExp 0 token

parsing :: String -> Maybe RegExp
parsing ['.'] = Just Any
parsing [char] = Just $ Normal char
parsing ('(':ts) = let (tl, par) = pickExp 0 ('(':ts) in case tl of
  [] -> parseRegExp par
  ('*':tl0) -> case tl0 of
    [] -> parseRegExp par >>= Just . ZeroOrMore
    _ -> Just $ Str [ZeroOrMore . fromJust $ parseRegExp par, fromJust $ parseRegExp tl0]
parsing ts = Just . Str . reverse $ foldl gen [] ts
  where gen (e:exps) '*' = ZeroOrMore e : exps
        gen exps '.' = Any : exps
        gen exps t = Normal t : exps


-- splitExp :: Int -> String -> (String, String)
splitExp 0 ('|':ts) = (ts, [])
splitExp p (s:ts0) = splitExp newp ts0 >>= (,) [] . (s:)
  where newp | s == '(' = p+1
             | s == ')' = p-1
             | otherwise = p
splitExp 0 _ = ([], [])

-- pickExp :: Int -> String -> (String, String)
pickExp 0 ('(':ts) = pickExp 1 ts
pickExp 1 (')':ts) = (ts, [])
pickExp 0 ts = (ts, [])
pickExp p (s:ts0) = pickExp newp ts0 >>= (,) [] . (s:)
  where newp | s == '(' = p+1
             | s == ')' = p-1
             | otherwise = p
