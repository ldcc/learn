--https://www.codewars.com/kata/5c8531a820089d04385a7faa/train/idris
--I've finally found some numeric proof that the Idris stdlib doesn't have. Let's do it.
--
--Enjoy proving a+a=b+b -> a=b.

module Proof

%default total
%access export

invert : (a : Nat) -> (b : Nat) -> (a + a = b + b) -> a = b
invert = ?prove_me
