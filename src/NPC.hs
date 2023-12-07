module NPC where

import Room

data NPC = NPC
  {
    name :: String,
    description :: String,
    hint :: String,
    npcLocation :: RoomName,
    looks :: Int -- The number of times the NPC has been in the room with the
                 -- "look" command happening
  }
  deriving (Show, Eq)

csprof :: NPC
csprof =
  NPC
    { name = "CS Professor",
      description = "You see a CS professor.",
      hint = "The CS professor says, " ++ 
             "\"Have you turned in your homework yet?\"",
      npcLocation = CrerarQuad,
      looks = 0
    }

deleteNPCByName :: String -> [NPC] -> [NPC]
deleteNPCByName _ [] = []
deleteNPCByName n (npc : npcs) =
  if n == name npc then npcs else npc : deleteNPCByName n npcs