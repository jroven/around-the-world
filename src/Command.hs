module Command where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]

itemNameP :: Parser ItemName
itemNameP =
  let eachItemName inm = pure inm <* string (show inm)
  in choice (map eachItemName itemNames)

nounPhrase_stub :: Parser ItemName
nounPhrase_stub = id <$> itemNameP

nounPhrase :: Parser [ItemName]
nounPhrase = sepBy1 (optional (char ' ') *> nounPhrase_stub) (char ',')

inventoryP :: Parser Command
inventoryP = pure Inventory <* string "inventory"

takeP :: Parser Command
takeP = Take <$> (string "take " *> nounPhrase)

exitP :: Parser Command
exitP = pure Exit <* string "exit" <|> string "quit"

dropP :: Parser Command
dropP = Drop <$> (string "drop " *> nounPhrase)

lookP :: Parser Command
lookP = pure Look <* string "look"

directionP :: Parser Direction
directionP =
  let eachDirection d = pure d <* string (show d)
  in choice (map eachDirection [N, S, E, W])

moveP :: Parser Command
moveP = Move <$> directionP

commandP :: Parser Command
commandP = choice [inventoryP, takeP, exitP, dropP, lookP, moveP]

conjunctionP :: Parser Conjunction
conjunctionP = sepBy1 commandP (string " and ") <* eof

parseInput :: String -> Maybe Conjunction
parseInput str =
  case parse conjunctionP str of
    Right conj -> Just conj
    _ -> Nothing
