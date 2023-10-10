module Player where

import Item
import Room

import Data.List

data Player
    = Player { inventory :: [ItemName]
             , maxWeight :: Integer
             , location :: RoomName }
    deriving (Show, Eq)

addItem :: ItemName -> Player -> Player
addItem i p = Player (i : (inventory p)) (maxWeight p) (location p)

removeItem :: ItemName -> Player -> Player
removeItem i p = Player (Data.List.delete i (inventory p))
                        (maxWeight p)
                        (location p)

newLocation :: RoomName -> Player -> Player
newLocation r p = Player (inventory p) (maxWeight p) r

isCarryingAnything :: Player -> Bool
isCarryingAnything p = inventory p /= []