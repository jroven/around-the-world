module GameIO where

import System.Random
import Control.Monad.State
import System.Exit
import System.IO

import GameState
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
    lift $ putStrLn "You successfully brought back all your souvenirs."
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
    lift $ putStrLn ("Travel the world to find the best souvenirs, then " ++
                    "bring them back to your collection in your bedroom.")

performCommand :: Command -> GameIO ()
performCommand com = do
    state <- get
    case com of
        Look -> do
            printDescription
            printObjects
            printExits
            checkNPCs
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

performConjunction :: Conjunction -> GameIO ()
performConjunction conj = do
    case conj of
        [] -> do
            st <- get
            case getNPCByName "CS Professor" st of
                Just csprof -> do
                    npcItemCheck csprof Homework csprofGive
                    npcRandomMove csprof
                Nothing -> lift $ pure ()
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
          elem LondonReturnTubeTicket (currentInventory state)
          &&
          elem AmsterdamPlaneTicket (currentInventory state)
          &&
          elem TeaSet (currentInventory state))
            (performCommand $ Move LHRreturn)

    when (currentRoom state == lhrPlane
          &&
          elem AmsterdamPlaneTicket (currentInventory state))
            (performCommand $ Move LHRAMS)

    when (currentRoom state == amsTrain
          &&
          elem AmsterdamTrainTicket (currentInventory state))
            (performCommand $ Move AmsterdamDowntown)

    when (currentRoom state == amsterdamCentraalStation
          &&
          elem AmsterdamReturnTrainTicket (currentInventory state)
          &&
          elem ParisPlaneTicket (currentInventory state)
          &&
          elem Stroopwafel (currentInventory state))
            (performCommand $ Move AMSreturn)

    when (currentRoom state == amsPlane
          &&
          elem ParisPlaneTicket (currentInventory state))
            (performCommand $ Move AMSCDG)

    when (currentRoom state == cdgTrain
          &&
          elem ParisTrainTicket (currentInventory state))
            (performCommand $ Move ParisDowntown)

    when (currentRoom state == parGareDuNord
          &&
          elem ParisReturnTrainTicket (currentInventory state)
          &&
          elem PraguePlaneTicket (currentInventory state)
          &&
          elem Croissant (currentInventory state))
            (performCommand $ Move CDGreturn)

    when (currentRoom state == cdgPlane
          &&
          elem PraguePlaneTicket (currentInventory state))
            (performCommand $ Move CDGPRG)

    when (currentRoom state == prgTrain
          &&
          elem PragueTrainTicket (currentInventory state))
            (performCommand $ Move PragueDowntown)

    when (currentRoom state == praMainStation
          &&
          elem PragueReturnTrainTicket (currentInventory state)
          &&
          elem IstanbulPlaneTicket (currentInventory state)
          &&
          elem RedGarnet (currentInventory state))
            (performCommand $ Move PRGreturn)

    when (currentRoom state == prgPlane
          &&
          elem IstanbulPlaneTicket (currentInventory state))
            (performCommand $ Move PRGIST)
    
    when (currentRoom state == istTaxi)
        (performCommand $ Move IstanbulDowntown)
    
    when (currentRoom state == istanbulTaxi
          &&
          elem TurkishDelight (currentInventory state)
          &&
          elem ChicagoPlaneTicket (currentInventory state))
            (performCommand $ Move ISTreturn)

    when (currentRoom state == istPlane
          &&
          elem ChicagoPlaneTicket (currentInventory state))
            (performCommand $ Move ISTORD)
    
    when (currentRoom state == ordLoadingZone
          &&
          elem TurkishDelight (currentInventory state))
            (performCommand $ Move HydePark)

checkNPCs :: GameIO ()
checkNPCs = do
    state <- get
    case getNPCByName "CS Professor" state of
        Just csprof -> do
            when (npcLocationCheck csprof state)
                (lift $ putStrLn $ description csprof)
            when (npcLocationCheck csprof state && looks csprof == 0)
                (lift $ putStrLn $ hint csprof)
            newState <- pure $ incrementNPClooks "CS Professor" state
            put newState
        Nothing -> lift $ pure ()

choose :: [a] -> IO a
choose xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

npcRandomMove :: NPC -> GameIO ()
npcRandomMove npc = do
    st <- get
    if npcLocationCheck npc st then lift (pure ()) else do
        exit <- lift $ choose $ normalExits $ getRoom (npcLocation npc) st
        newSt <- pure $ replaceNPClooks (name npc) (0)
            (npcMove npc (fst exit) st)
        put newSt

npcItemCheck :: NPC -> ItemName -> GameIO () -> GameIO ()
npcItemCheck npc i action = do
    st <- get
    if npcLocationCheck npc st && elem i (objects $ currentRoom st)
        then do
            action
        else lift $ pure ()

csprofGive :: GameIO ()
csprofGive = do
    st <- get
    put st
      { gmap =
          setRoomMap
            (rname $ currentRoom st)
            (Room.removeItem Homework
                (Room.addItem LondonPlaneTicket $ currentRoom st))
            (gmap st)
      }
    lift $ putStrLn "The CS professor sees your homework and takes it."
    lift $ putStrLn ("The CS professor says: \"Excellent work! " ++
                    "Here is a token of my appreciation.\"")
    lift $ putStrLn "The CS professor drops something for you."

npcLocationCheck :: NPC -> GameState -> Bool
npcLocationCheck npc st = currentRoom st == getRoom (npcLocation npc) st

csprofHint :: GameIO ()
csprofHint = do
    lift $ putStrLn "You see a CS professor."
    lift $ putStrLn
          "The CS professor says, \"Have you turned in your homework yet?\""
