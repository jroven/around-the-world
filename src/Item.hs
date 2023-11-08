module Item where

import Data.Map qualified as M
import Data.Char

data ItemName
  = Pot
  | Jug
  | Sandbag
  | Stove
  | Couch
  | Tarragon
  | Beans
  | Grill
  | Bed
  deriving (Eq, Ord)

instance Show ItemName where
  show Pot = "pot"
  show Jug = "jug"
  show Sandbag = "sandbag"
  show Stove = "stove"
  show Couch = "couch"
  show Tarragon = "tarragon"
  show Beans = "beans"
  show Grill = "grill"
  show Bed = "bed"

type Universe = M.Map ItemName Item

data Item = Item {iname :: ItemName, weight :: Integer} deriving (Show)

mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (mkUniverseHelper items)

mkUniverseHelper :: [Item] -> [(ItemName, Item)]
mkUniverseHelper [] = []
mkUniverseHelper (x : xs) = ((iname x, x) : mkUniverseHelper xs)

-- List of predefined Items
pot :: Item
pot = Item {iname = Pot, weight = 30}

jug :: Item
jug = Item {iname = Jug, weight = 18}

sandbag :: Item
sandbag = Item {iname = Sandbag, weight = 40}

stove :: Item
stove = Item {iname = Stove, weight = 2000}

couch :: Item
couch = Item {iname = Couch, weight = 2500}

tarragon :: Item
tarragon = Item {iname = Tarragon, weight = 1}

beans :: Item
beans = Item {iname = Beans, weight = 6}

grill :: Item
grill = Item {iname = Grill, weight = 2000}

bed :: Item
bed = Item {iname = Bed, weight = 3500}

-- Universe
univ :: Universe
univ =
  mkUniverse
    [ stove,
      pot,
      couch,
      sandbag,
      jug,
      grill,
      bed,
      tarragon,
      beans
    ]

-- Item Names
itemNames :: [ItemName]
itemNames = map iname $ M.elems univ