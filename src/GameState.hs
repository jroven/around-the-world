module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room

data GameState = GameState
  { message :: Maybe String,
    gmap :: GameMap,
    universe :: Universe,
    player :: Player
  }
  deriving Show

mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList (mkMapHelper rooms)

mkMapHelper :: [Room] -> [(RoomName, Room)]
mkMapHelper [] = []
mkMapHelper (x : xs) = ((rname x, x) : mkMapHelper xs)

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState
initialState =
    GameState
    {
        message = Nothing,
        gmap = gameMap,
        universe = univ,
        player = you
    }

data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap oldRname newRoom gm =
    M.insert (rname newRoom) newRoom $ M.delete oldRname gm

setMessage :: String -> GameState -> GameState
setMessage s st =
    GameState
    {
        message = Just s,
        gmap = gmap st,
        universe = universe st,
        player = player st
    }

currentInventory :: GameState -> [ItemName]
currentInventory st = inventory $ player st

currentRoom :: GameState -> Room
currentRoom st = getRoom (location $ player st) st

nearbyObjects :: GameState -> [ItemName]
nearbyObjects st = objects $ currentRoom st

takeItem :: ItemName -> GameState -> GameState
takeItem i st =
    setMessage
    ("You take the " ++ i ++ ".")
    GameState
    {
      message = Nothing,
      gmap =
          setRoomMap
          (rname $ currentRoom st)
          (Room.removeItem i $ currentRoom st)
          (gmap st),
      universe = universe st,
      player = Player.addItem i $ player st
    }

dropItem :: ItemName -> GameState -> GameState
dropItem i st =
    setMessage
    ("You drop the " ++ i ++ ".")
    GameState
    {
      message = Nothing,
      gmap =
          setRoomMap
          (rname $ currentRoom st)
          (Room.addItem i $ currentRoom st)
          (gmap st),
      universe = universe st,
      player = Player.removeItem i $ player st
    }