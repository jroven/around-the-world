module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item

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
        [] -> pure()
        (i : is) -> do
            lift (putStrLn (show i))
            printObjectsRecurse is

printExits :: GameIO ()
printExits = do
    state <- get
    case exits (currentRoom state) of
        [] -> pure ()
        xs -> printExitsHelper xs

-- Helper for printExits
printExitsHelper xs = do
    lift $ putStrLn "There are exits in the following directions:"
    printExitsRecurse xs

-- Another helper for printExits
printExitsRecurse xs = do
    case xs of
        [] -> pure()
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

performConjunction :: Conjunction -> GameIO ()
performConjunction conj = do
    case conj of
        [] -> pure ()
        (c : cs) -> do
            performCommand c
            performConjunction cs

parseConjunction :: String -> GameIO ()
parseConjunction str = do
    case parseInput str of
        Nothing -> syntaxError
        Just conj -> performConjunction conj

repl :: GameIO ()
repl = do
    prompt
    str <- lift getLine
    parseConjunction str
    checkGameOver