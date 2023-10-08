module Player where

import Item
import Room

data Player
    = Player { inventory :: [ItemName]
             , maxWeight :: Integer
             , location :: RoomName }
    deriving (Show, Eq)

addItem :: ItemName -> Player -> Player
addItem i p = undefined