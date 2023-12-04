module Direction where

data Direction
  = N
  | S
  | E
  | W
  | ORD -- from Hyde Park to ORD
  | HydePark -- from ORD to Hyde Park
  | LHRfly -- from ORD to LHR
  | LondonDowntown -- from LHR to London
  | LHRreturn -- from London to LHR
  | LHRAMS -- from London to Amsterdam
  deriving (Eq)

instance Show Direction where
  show N = "north"
  show S = "south"
  show E = "east"
  show W = "west"
  show ORD = "ord"
  show HydePark = "hyde park"
  show LHRfly = "lhr fly"
  show LondonDowntown = "londondownton"
  show LHRreturn = "lhr return"
  show LHRAMS = "lhr ams"