module GameIO where

import System.Random
import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item
import NPC
import Direction

type GameIO a = StateT GameState IO a

prompt :: GameIO ()
prompt = do
    lift $ putStr "-> " >> hFlush stdout

printMessage :: GameIO ()
printMessage = do
    state <- get
    case message state of
        Nothing -> pure ()
        Just msg -> printMessageHelper state msg

-- Helper for printMessage
printMessageHelper state msg = do
    lift $ putStrLn msg
    put (state { message = Nothing })

printDescription :: GameIO ()
printDescription = do
    state <- get
    lift $ putStrLn (desc (currentRoom state))

printObjects :: GameIO ()
printObjects = do
    state <- get
    case objects (currentRoom state) of
        [] -> pure ()
        xs -> printObjectsHelper xs

-- Helper for printObjects        
printObjectsHelper :: (Monad (t IO), MonadTrans t) => [ItemName] -> t IO ()
printObjectsHelper xs = do
    lift $ putStrLn "You see the following objects:"
    printObjectsRecurse xs

-- Another helper for printObjects
printObjectsRecurse :: (Monad (t IO), MonadTrans t) => [ItemName] -> t IO ()
printObjectsRecurse xs = do
    case xs of
        [] -> pure ()
        (i : is) -> do
            lift (putStrLn (show i))
            printObjectsRecurse is

printExits :: GameIO ()
printExits = do
    state <- get
    case normalExits (currentRoom state) of
        [] -> pure ()
        xs -> printExitsHelper xs

-- Helper for printExits
printExitsHelper xs = do
    lift $ putStrLn "There are exits in the following directions:"
    printExitsRecurse xs

-- Another helper for printExits
printExitsRecurse xs = do
    case xs of
        [] -> pure ()
        (e : es) -> do
            lift (putStrLn (show (fst e)))
            printExitsRecurse es

printInventory :: GameIO ()
printInventory = do
    state <- get
    case currentInventory state of
        [] -> lift $ putStrLn "You aren't carrying anything."
        xs -> printInventoryHelper xs

-- Helper for printInventory
printInventoryHelper xs = do
    lift $ putStrLn "You are carrying the following items:"
    printObjectsRecurse xs

actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList action xs = do
    state <- get
    actionOverListHelper state action xs

actionOverListHelper :: GameState
                     -> (ItemName -> GameState -> GameState)
                     -> [ItemName]
                     -> GameIO ()
actionOverListHelper state action xs = do
    case xs of
        [] -> pure ()
        (i : is) -> do
            state <- pure (action i state)
            case message state of
                Nothing -> pure ()
                Just msg -> printMessageHelper state msg
            actionOverListHelper state action is

finishGame :: GameIO ()
finishGame = do
    lift $ putStrLn "You successfully brought the jug into the yard."
    lift $ putStrLn "Congratulations! You win!"
    lift exitSuccess

exit :: GameIO ()
exit = do
    lift $ putStrLn "Goodbye!"
    lift exitSuccess

checkGameOver :: GameIO ()
checkGameOver = do
    state <- get
    if haveWonGame state then finishGame else pure ()

syntaxError :: GameIO ()
syntaxError = do
    lift $ putStrLn "I don't understand that."

opening :: GameIO ()
opening = do
    lift $ putStrLn "Welcome to Functional Adventure!"

performCommand :: Command -> GameIO ()
performCommand com = do
    state <- get
    case com of
        Look -> do
            printDescription
            printObjects
            printExits
        Move d -> do
            state <- pure (move d state)
            case message state of
                Nothing -> pure ()
                Just msg -> printMessageHelper state msg
        Inventory -> do
            printInventory
        Take xs -> do
            actionOverList takeItem xs
        Drop xs -> do
            actionOverList dropItem xs
        Exit -> do
            exit
    checkChangeMap
    removeUsedObjects

performConjunction :: Conjunction -> GameIO ()
performConjunction conj = do
    case conj of
        [] -> do
            npcRandomMove csprof
        (c : cs) -> do
            performCommand c
            performConjunction cs

parseConjunction :: String -> GameIO ()
parseConjunction str = do
    case parseInput str of
        Nothing -> syntaxError
        Just conj -> do
            performConjunction conj

repl :: GameIO ()
repl = do
    prompt
    str <- lift getLine
    parseConjunction str
    checkGameOver

removeUsedObjects :: GameIO ()
removeUsedObjects = do
    state <- get
    when (currentRoom state == lhrGate
          &&
          elem LondonPlaneTicket (currentInventory state))
            (lift $ pure ()) -- FIX THIS TO ACTUALLY MAKE IT REMOVE TICKET

checkChangeMap :: GameIO ()
checkChangeMap = do
    state <- get
    when (currentRoom state == yardTaxi
          &&
          elem LondonPlaneTicket (currentInventory state))
            (performCommand $ Move ORD)
    when (currentRoom state == ordPlane
          &&
          elem LondonPlaneTicket (currentInventory state))
            (performCommand $ Move LHRfly)
    when (currentRoom state == lhrTrain
          &&
          elem LondonTubeTicket (currentInventory state))
            (performCommand $ Move LondonDowntown)
    when (currentRoom state == lonWestminsterTubeStation
          &&
          elem LondonReturnTubeTicket (currentInventory state))
            (performCommand $ Move LHRreturn)
    when (currentRoom state == lhrPlane
          &&
          elem AmsterdamPlaneTicket (currentInventory state))
            (performCommand $ Move LHRAMS)

choose :: [a] -> IO a
choose xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

npcRandomMove :: NPC -> GameIO ()
npcRandomMove npc = do
    st <- get
    exit <- lift $ choose $ exits $ getRoom (npcLocation npc) st
    st <- pure $ npcMove npc (fst exit) st
    pure ()

npcItemCheck :: NPC -> ItemName -> GameIO () -> GameIO ()
npcItemCheck npc i action = do
    st <- get
    if npcLocationCheck npc st && elem i (objects $ currentRoom st)
        then action
        else lift $ putStrLn "not there (TEST)"

npcLocationCheck :: NPC -> GameState -> Bool
npcLocationCheck npc st = currentRoom st == getRoom (npcLocation npc) st

csprofHint :: GameIO ()
csprofHint = do
    lift $ putStrLn "You see a CS professor."
    lift $ putStrLn
          "The CS professor says, \"Have you turned in your homework yet?\""