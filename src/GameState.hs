module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room

type Error a = Either String a

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
  case takeItemHelper i st of
    Left str -> setMessage str st
    Right newState ->
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
    
takeItemHelper :: ItemName -> GameState -> Error GameState
takeItemHelper i st = do
  alreadyHave <- alreadyHaveTakeCheck i st
  inRoom <- inRoomTakeCheck i alreadyHave
  tooHeavy <- weightCheck i inRoom
  pure tooHeavy

dropItem :: ItemName -> GameState -> GameState
dropItem i st =
  case dropItemHelper i st of
    Left str -> setMessage str st
    Right newState ->
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

dropItemHelper :: ItemName -> GameState -> Error GameState
dropItemHelper i st = do
  anywhere <- anywhereDropCheck i st
  inRoom <- inRoomDropCheck i anywhere
  pure inRoom

inventoryWeight :: GameState -> Integer
inventoryWeight st = item_sum (inventory $ player st) st where
  item_sum [] _ = 0
  item_sum (i : is) st = weight (getObject i st) + item_sum is st

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck i st =
  if elem i $ currentInventory st
    then Left $ "You are already carrying the " ++ i ++ "."
    else Right st

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck i st =
  if elem i $ nearbyObjects st
    then Right st
    else Left $ "There is no " ++ i ++ " in this room."

weightCheck :: ItemName -> GameState -> Error GameState
weightCheck i st =
  if inventoryWeight st + weight (getObject i st) > maxWeight (player st)
    then Left "That's too much weight for you to carry."
    else Right st

anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck i st =
  if elem i (currentInventory st) || elem i (nearbyObjects st)
    then Right st
    else Left $ "What do you mean, drop the " ++ i ++ "?"

inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck i st =
  if elem i $ nearbyObjects st
    then Left $ "You aren't carrying the " ++ i ++ "."
    else Right st

roomHasObjects :: GameState -> Bool
roomHasObjects st = hasObjects $ currentRoom st

destinationName :: Direction -> Room -> Maybe RoomName
destinationName d r = destinationNameHelper d $ exits r where
  destinationNameHelper :: Direction -> [Exit] -> Maybe RoomName
  destinationNameHelper _ [] = Nothing
  destinationNameHelper d (exit : exits) =
    if fst exit == d then Just (snd exit) else destinationNameHelper d exits

move :: Direction -> GameState -> GameState
move d st =
  case destinationName d (currentRoom st) of
    Just r ->
      setMessage
      ("You go " ++ show d ++ ".")
      st { player = (player st) { location = r } }
    Nothing ->
      setMessage
      "There is no exit in that direction."
      st

haveWonGame :: GameState -> Bool
haveWonGame st =
  let p = player st
  in location p == "yard" && elem "jug" (inventory p)