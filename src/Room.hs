module Room where

import Direction
import Item
import Data.List

type RoomName = String

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
    { rname = "kitchen",
      desc = "You are in a small kitchen.",
      exits =
        [ (N, "living room"),
          (S, "yard"),
          (E, "pantry")
        ],
      objects =
        [ "pot",
          "stove"
        ]
    }

pantry :: Room
pantry =
  Room
    { rname = "pantry",
      desc = "You are in a pantry.",
      exits =
        [ (W, "kitchen")
        ],
      objects =
        [ "tarragon",
          "beans"
        ]
    }

yard :: Room
yard =
  Room
    { rname = "yard",
      desc = "You are in a small yard.",
      exits =
        [ (N, "kitchen")
        ],
      objects =
        [ "grill"
        ]
    }

livingRoom :: Room
livingRoom =
  Room
    { rname = "living room",
      desc = "You are in a small living room.",
      exits =
        [ (N, "bedroom"),
          (S, "kitchen")
        ],
      objects =
        [ "couch",
          "jug",
          "sandbag"
        ]
    }

bedroom :: Room
bedroom =
  Room
    { rname = "bedroom",
      desc = "You are in a small bedroom.",
      exits =
        [ (S, "living room")
        ],
      objects =
        [ "bed"
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