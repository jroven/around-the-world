module Item where

import Data.Map qualified as M
import Data.Char

data ItemName
  = Pot
  | Homework
  | Rake
  | Knife
  | Fork
  | Bed
  | LondonPlaneTicket
  | LondonTubeTicket
  | LondonReturnTubeTicket
  | AmsterdamPlaneTicket
  deriving (Eq, Ord)

instance Show ItemName where
  show Pot = "pot"
  show Homework = "homework"
  show Rake = "rake"
  show Knife = "knife"
  show Fork = "fork"
  show Bed = "bed"
  show LondonPlaneTicket = "plane ticket london"
  show LondonTubeTicket = "tube ticket downtown"
  show LondonReturnTubeTicket = "tube ticket airport"
  show AmsterdamPlaneTicket = "plane ticket amsterdam"

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

homework :: Item
homework = Item {iname = Homework, weight = 5}

rake :: Item
rake = Item {iname = Rake, weight = 20}

knife :: Item
knife = Item {iname = Knife, weight = 6}

fork :: Item
fork = Item {iname = Fork, weight = 6}

bed :: Item
bed = Item {iname = Bed, weight = 200}

londonPlaneTicket :: Item
londonPlaneTicket = Item {iname = LondonPlaneTicket, weight = 0}

londonTubeTicket :: Item
londonTubeTicket = Item {iname = LondonTubeTicket, weight = 1}

londonReturnTubeTicket :: Item
londonReturnTubeTicket = Item {iname = LondonReturnTubeTicket, weight = 1}

amsterdamPlaneTicket :: Item
amsterdamPlaneTicket = Item {iname = AmsterdamPlaneTicket, weight = 0}

-- Universe
univ :: Universe
univ =
  mkUniverse
    [ pot,
      homework,
      rake,
      knife,
      fork,
      bed,
      londonPlaneTicket,
      londonTubeTicket,
      londonReturnTubeTicket,
      amsterdamPlaneTicket
    ]

-- Item Names
itemNames :: [ItemName]
itemNames = map iname $ M.elems univ