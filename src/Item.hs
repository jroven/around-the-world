module Item where

import qualified Data.Map as M

type ItemName = String

type Universe = M.Map String Item

data Item = Item { iname :: ItemName, weight :: Integer } deriving Show

mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (mkUniverseHelper items)

mkUniverseHelper :: [Item] -> [(ItemName, Item)]
mkUniverseHelper [] = []
mkUniverseHelper (x : xs) = ((iname x, x) : mkUniverseHelper xs)