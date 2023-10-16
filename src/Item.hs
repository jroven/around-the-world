module Item where

import Data.Map qualified as M

type ItemName = String

type Universe = M.Map String Item

data Item = Item {iname :: ItemName, weight :: Integer} deriving (Show)

mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (mkUniverseHelper items)

mkUniverseHelper :: [Item] -> [(ItemName, Item)]
mkUniverseHelper [] = []
mkUniverseHelper (x : xs) = ((iname x, x) : mkUniverseHelper xs)

-- List of predefined Items
pot :: Item
pot = Item {iname = "pot", weight = 30}

jug :: Item
jug = Item {iname = "jug", weight = 18}

sandbag :: Item
sandbag = Item {iname = "sandbag", weight = 40}

stove :: Item
stove = Item {iname = "stove", weight = 2000}

couch :: Item
couch = Item {iname = "couch", weight = 2500}

tarragon :: Item
tarragon = Item {iname = "tarragon", weight = 1}

beans :: Item
beans = Item {iname = "beans", weight = 6}

grill :: Item
grill = Item {iname = "grill", weight = 2000}

bed :: Item
bed = Item {iname = "bed", weight = 3500}

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