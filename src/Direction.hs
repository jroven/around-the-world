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
  | LHRAMS -- from LHR to AMS
  | AmsterdamDowntown -- from AMS to Amsterdam
  | AMSreturn -- from Amsterdam to AMS
  | AMSCDG -- from AMS to CDG
  | ParisDowntown -- from CDG to Paris
  | CDGreturn -- from Paris to CDG
  | CDGPRG -- from CDG to PRG
  | PragueDowntown -- from PRG to Prague
  | PRGreturn -- from Prague to PRG
  | PRGIST -- from PRG to IST
  | IstanbulDowntown -- from IST to Istanbul
  | ISTreturn -- from Istanbul to IST
  | ISTORD -- from IST to ORD
  deriving (Eq)

instance Show Direction where
  show N = "north"
  show S = "south"
  show E = "east"
  show W = "west"
  show ORD = "ord"
  show HydePark = "hyde park"
  show LHRfly = "lhr fly"
  show LondonDowntown = "london downton"
  show LHRreturn = "lhr return"
  show LHRAMS = "lhr ams"
  show AmsterdamDowntown = "amsterdam downtown"
  show AMSreturn = "ams return"
  show AMSCDG = "ams cdg"
  show ParisDowntown = "paris downtown"
  show CDGreturn = "cdg return"
  show CDGPRG = "cdg prg"
  show PragueDowntown = "prague downtown"
  show PRGreturn = "prg return"
  show PRGIST = "prg ist"
  show IstanbulDowntown = "istanbul downtown"
  show ISTreturn = "ist return"
  show ISTORD = "ist ord"