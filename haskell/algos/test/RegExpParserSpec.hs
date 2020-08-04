module RegExpParserSpec (spec) where

import RegExpParser
import Test.Hspec
import Text.Printf
import Control.Monad

validate exs =
 forM_ exs $ \(input, output) ->
    it (printf "should return %s given %s as input" (show output) (show input)) $ do
       let res = parseRegExp input
       res `shouldBe` output

spec :: Spec
spec = do
  describe "Precedence examples" $ do
    let exs =
          [ ("ab*", Just (Str [Normal 'a', ZeroOrMore (Normal 'b')]))
          , ("(ab)*", Just ( ZeroOrMore (Str [Normal 'a', Normal 'b'])))
          , ("ab|a", Just (Or (Str [Normal 'a',Normal 'b']) (Normal 'a')))
          , ("a(b|a)", Just (Str [Normal 'a',Or (Normal 'b') (Normal 'a')]))
          , ("a|b*", Just (Or (Normal 'a') (ZeroOrMore (Normal 'b'))))
          , ("(a|b)*", Just (ZeroOrMore (Or (Normal 'a') (Normal 'b'))))
          ]
    validate exs
  describe "The other examples" $ do
    let exs =
          [ ("a", Just (Normal 'a'))
          , ("ab", Just (Str [ Normal 'a', Normal 'b']))
          , ("a.*", Just (Str [ Normal 'a', ZeroOrMore Any ]))
          , ("(a.*)|(bb)", Just (Or (Str [Normal 'a', ZeroOrMore Any]) (Str [Normal 'b', Normal 'b'])))
          ]
    validate exs
  describe "Invalid examples" $ do
    let exs =
          [ ("", Nothing)
          , ("(", Nothing)
          , (")(", Nothing)
          , ("*", Nothing)
          ]
    validate exs
  describe "Complex examples" $ do
    let exs =
          [ ("((aa)|ab)*|a", Just (Or (ZeroOrMore (Or (Str [Normal 'a',Normal 'a']) (Str [Normal 'a',Normal 'b']))) (Normal 'a')))
          , ("((a.)|.b)*|a", Just (Or (ZeroOrMore (Or (Str [Normal 'a',Any]) (Str [Any,Normal 'b']))) (Normal 'a')))
          ]
    validate exs