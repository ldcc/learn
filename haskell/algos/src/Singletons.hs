{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
-- to be defined

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index = undefined

replicate :: s -> SNat a -> Vec s a
replicate = undefined

---- Both vectors must be of equal length
--zipWith :: ??
--zipWith = undefined
--
--(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
--a ++ b = undefined
--
---- The semantics should match that of take for normal lists.
--take :: ??
--take = undefined
--
---- The semantics should match that of drop for normal lists.
--drop :: ??
--drop = undefined
--
--head :: ??
--head = undefined
--
--tail :: ??
--tail = xs
