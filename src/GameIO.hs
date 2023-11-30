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
    recurse xs

-- Another helper for printObjects
recurse :: (Monad (t IO), MonadTrans t) => [ItemName] -> t IO ()
recurse xs = do
    case xs of
        [] -> pure()
        (i : is) -> do
            lift (putStrLn (show i))
            recurse is