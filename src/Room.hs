module Room where

import Direction
import Item
import Data.List

data RoomName
  = Kitchen
  | Pantry
  | Yard
  | LivingRoom
  | Bedroom
  deriving (Eq, Ord)

instance Show RoomName where
  show Kitchen = "kitchen"
  show Pantry = "pantry"
  show Yard = "yard"
  show LivingRoom = "living room"
  show Bedroom = "bedroom"

type Exit = (Direction, RoomName)

data Room = Room
  { rname :: RoomName,
    desc :: String,
    exits :: [Exit],
    objects :: [ItemName]
  }
  deriving (Show, Eq)

-- List of predefined Rooms
kitchen :: Room
kitchen =
  Room
    { rname = Kitchen,
      desc = "You are in a small kitchen.",
      exits =
        [ (N, LivingRoom),
          (S, Yard),
          (E, Pantry)
        ],
      objects =
        [ Pot,
          Stove
        ]
    }

pantry :: Room
pantry =
  Room
    { rname = Pantry,
      desc = "You are in a pantry.",
      exits =
        [ (W, Kitchen)
        ],
      objects =
        [ Tarragon,
          Beans
        ]
    }

yard :: Room
yard =
  Room
    { rname = Yard,
      desc = "You are in a small yard.",
      exits =
        [ (N, Kitchen)
        ],
      objects =
        [ Grill
        ]
    }

livingRoom :: Room
livingRoom =
  Room
    { rname = LivingRoom,
      desc = "You are in a small living room.",
      exits =
        [ (N, Bedroom),
          (S, Kitchen)
        ],
      objects =
        [ Couch,
          Jug,
          Sandbag
        ]
    }

bedroom :: Room
bedroom =
  Room
    { rname = Bedroom,
      desc = "You are in a small bedroom.",
      exits =
        [ (S, LivingRoom)
        ],
      objects =
        [ Bed
        ]
    }

-- Room Names
roomNames :: [RoomName]
roomNames = map rname rooms

rooms :: [Room]
rooms =
  [ kitchen,
    pantry,
    yard,
    livingRoom,
    bedroom
  ]

addItem :: ItemName -> Room -> Room
addItem i r =
  Room
  {
    rname = rname r,
    desc = desc r,
    exits = exits r,
    objects = i : objects r
  }

removeItem :: ItemName -> Room -> Room
removeItem i r =
  Room
  {
    rname = rname r,
    desc = desc r,
    exits = exits r,
    objects = Data.List.delete i (objects r)
  }

allRooms :: [Room]
allRooms = rooms

hasObjects :: Room -> Bool
hasObjects r = not $ null (objects r)