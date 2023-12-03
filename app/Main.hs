module Main where

import Control.Monad.State

import GameState
import GameIO

foreverRepl :: GameState -> IO ()
foreverRepl state = do
    newstate <- execStateT repl state
    foreverRepl newstate

main :: IO ()
main = do
    evalStateT opening initialState
    foreverRepl initialState