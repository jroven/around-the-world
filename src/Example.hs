module Example where

import Data.Map qualified as M
import Data.List
import System.Random

import Item
import Direction
import Room
import Player

choose :: [a] -> IO a
choose xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

exampleList :: IO a -> IO Int -> IO [a]
exampleList val len = do
  l <- len
  exampleListHelper val l

exampleListHelper :: IO a -> Int -> IO [a]
exampleListHelper _ 0 = do return []
exampleListHelper val len = do
  v <- val
  rest <- exampleListHelper val (len - 1)
  return $ v : rest

class Example a where
  example :: IO a

instance Example Item where
  example :: IO Item
  example = do
    name <- choose itemNames
    weight <- randomRIO (1, 1000)
    return Item {iname = name, weight = weight}

instance Example Direction where
  example :: IO Direction
  example = do
    choose [N, S, E, W]

exitExample :: IO Exit
exitExample = do
  d <- example :: IO Direction
  r <- choose roomNames
  return (d, r)

instance Example Room where
  example :: IO Room
  example = do
    r <- choose roomNames
    exits <- exampleList exitExample (randomRIO (2,4))
    objects <- exampleList (choose itemNames) (randomRIO (2,5))
    return Room {
      rname = r,
      desc = "You are in the " ++ r ++ ".  It is a randomly-generated room.",
      exits = exits,
      objects = objects
    }

instance Example Player where
  example :: IO Player
  example = do
    inv <- exampleList (choose itemNames) (randomRIO (0,10))
    mw <- randomRIO (3482,3518)
    loc <- choose roomNames
    return Player {
      inventory = removeDuplicates inv,
      maxWeight = mw,
      location = loc
    }

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)