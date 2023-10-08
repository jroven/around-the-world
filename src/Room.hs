module Room where

import Item
import Direction

type RoomName = String

type Exit = (Direction, RoomName)

data Room
    = Room { rname :: RoomName
           , desc :: String
           , exits :: [Exit]
           , objects :: [ItemName] }
    deriving (Show, Eq)