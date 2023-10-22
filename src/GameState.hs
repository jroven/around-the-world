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