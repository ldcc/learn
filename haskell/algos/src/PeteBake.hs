module PeteBake where

import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, (!?))

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage0 = fromMaybe 0 $ minimum [(`div` a) <$> (storage1 !? i) | (i, a) <- recipe]
  where storage1 = fromList storage0
