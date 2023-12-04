module NPC where

import Room

data NPC = NPC
  {
    name :: String,
    npcLocation :: RoomName
  }
  deriving (Show, Eq)

csprof :: NPC
csprof =
  NPC
    { name = "CS Professor",
      npcLocation = CrerarQuad
    }