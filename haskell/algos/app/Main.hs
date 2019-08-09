module Main where

import Data.Either (fromRight)
import SimpleInteractiveInterpreter

main :: IO ()
--main = putStrLn . show $ input "" newInterpreter
main = do
  let int0 = newInterpreter
  let ret1 = input "x = 1" int0
  putStrLn . show $ fst <$> ret1
  let ret2 = ret1 >>= input "fn add x y => x + z" . snd
  putStrLn $ show $ fst <$> ret2
  let ret3 = ret1 >>= input "fn add x x => x + x" . snd
  putStrLn $ show $ fst <$> ret3

--class Monad m => MonadT m where
--  (-?) :: Monad n => m a -> n b -> n b -> n b
--  (\?) :: Monad n => m a -> (a -> n b) -> n b -> n b
--
--instance MonadT Maybe where
--  Nothing -? _ = id
--  _ -? y = \ _ -> y
--  Nothing \? _ = id
--  (Just x) \? f = \ _ -> f x
--
--(-:) :: Monad m => (a -> m b) -> a -> m b
--f -: x = return x >>= f