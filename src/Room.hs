module Room where

import Direction
import Item
import Data.List

data RoomName
  -- Hyde Park
  = Backyard
  | Kitchen
  | LivingRoom
  | Bedroom
  | WalkInCloset
  | Bathroom
  | Foyer
  | FrontYard
  | YardTaxi
  | FiftyFourth
  | FiftyFifth
  | NorthResidenceHall
  | Insomnia
  | Ratner
  | MaxPEast
  | BreakupForest
  | MaxPCentral
  | MaxPWest
  | UniversityChurch
  | Bartlett
  | Regenstein
  | Mansueto
  | Kersten
  | Reynolds
  | HutchQuad
  | Quad
  | Bookstore
  | CrerarQuad
  | HarperQuad
  | Cobb
  
  -- ORD
  | OrdLoadingZone
  | OrdLobby
  | OrdSecurity
  | OrdInternationalTerminal
  | OrdBurgerRestaurant
  | OrdLounge
  | OrdCoffeeShop
  | OrdGate
  | OrdPlane

  -- LHR
  | LhrPlane
  | LhrGate
  | LhrCustoms
  | LhrSecurity
  | LhrLobby
  | LhrAirportTubeStation
  | LhrTrain

  -- London
  | LonWestminsterTubeStation
  | LonWestminsterAbbey
  | LonRoyalBotanicGardens
  | LonHousesOfParliament
  | LonLondonEye
  | LonShakespearesGlobeTheatre
  | LonLondonBridge
  | LonSkyGarden
  | LonStPaulsCathedral
  | LonFilmMuseum
  | LonTrafalgarSquare
  | LonZoo
  | LonRegentsPark
  | LonMadameTussauds

  -- AMS
  | AmsPlane
  | AmsGate
  | AmsCustoms
  | AmsSecurity
  | AmsLobby
  | AmsAirportStation
  | AmsTrain

  -- Amsterdam
  | AmsterdamCentraalStation
  | AmsterdamNemoScienceMuseum
  | AmsterdamAnneFrankHouse
  | AmsterdamOldChurch
  | AmsterdamRoyalPalace
  | AmsterdamOosterpark
  | AmsterdamHortusBotanicus
  | AmsterdamRembrandtHouse
  | AmsterdamMuseum
  | AmsterdamHuisMarseille
  | AmsterdamFlowerMarket
  | AmsterdamRijksmuseum
  | AmsterdamVondelpark
  | AmsterdamHeinekenExperience
  | AmsterdamVanGoghMuseum
  deriving (Eq, Ord)

instance Show RoomName where
  -- Hyde Park
  show Backyard = "backyard"
  show Kitchen = "kitchen"
  show LivingRoom = "living room"
  show Bedroom = "bedroom"
  show WalkInCloset = "walk in closet"
  show Bathroom = "bathroom"
  show Foyer = "foyer"
  show FrontYard = "front yard"
  show YardTaxi = "yard taxi"
  show FiftyFourth = "54th street"
  show FiftyFifth = "55th street"
  show NorthResidenceHall = "North residence hall"
  show Insomnia = "Insomnia"
  show Ratner = "Ratner"
  show MaxPEast = "Max Palevsky East"
  show BreakupForest = "breakup forest"
  show MaxPCentral = "Max Palevsky Central"
  show MaxPWest = "Max Palevsky"
  show UniversityChurch = "university church"
  show Bartlett = "Bartlett"
  show Regenstein = "Regenstein"
  show Mansueto = "Mansueto"
  show Kersten = "Kersten"
  show Reynolds = "Reynolds"
  show HutchQuad = "Hutch quad"
  show Quad = "quad"
  show Bookstore = "bookstore"
  show CrerarQuad = "Crerar quad"
  show HarperQuad = "Harper quad"
  show Cobb = "Cobb"

  -- ORD
  show OrdLoadingZone = "ORD loading zone"
  show OrdLobby = "ORD lobby"
  show OrdSecurity = "ORD security"
  show OrdInternationalTerminal = "ORD international terminal"
  show OrdBurgerRestaurant = "ORD burger restaurant"
  show OrdLounge = "ORD lounge"
  show OrdCoffeeShop = "ORD coffee shop"
  show OrdGate = "ORD gate"
  show OrdPlane = "ORD plane"

  -- LHR
  show LhrPlane = "LHR plane"
  show LhrGate = "LHR gate"
  show LhrCustoms = "LHR customs"
  show LhrSecurity = "LHR security"
  show LhrLobby = "LHR lobby"
  show LhrAirportTubeStation = "LHR airport tube station"
  show LhrTrain = "LHR train"

  -- London
  show LonWestminsterTubeStation = "Westminster tube station"
  show LonWestminsterAbbey = "Westminster Abbey"
  show LonRoyalBotanicGardens = "royal botanic gardens"
  show LonHousesOfParliament = "houses of parilament"
  show LonLondonEye = "London Eye"
  show LonShakespearesGlobeTheatre = "Shakespeare's Globe Theatre"
  show LonLondonBridge = "London Bridge"
  show LonSkyGarden = "sky garden"
  show LonStPaulsCathedral = "St Paul's Cathedral"
  show LonFilmMuseum = "film museum"
  show LonTrafalgarSquare = "Trafalgar Square"
  show LonZoo = "zoo"
  show LonRegentsPark = "Regent's Park"
  show LonMadameTussauds = "Madame Tussauds"

  -- AMS
  show AmsPlane = "AMS Plane"
  show AmsGate = "AMS Gate"
  show AmsCustoms = "AMS Customs"
  show AmsSecurity = "AMS Security"
  show AmsLobby = "AMS Lobby"
  show AmsAirportStation = "AMS Airport Station"
  show AmsTrain = "AMS Train"

  -- Amsterdam
  show AmsterdamCentraalStation = "Centraal Station"
  show AmsterdamNemoScienceMuseum = "Nemo Science Museum"
  show AmsterdamAnneFrankHouse = "Anne Frank House"
  show AmsterdamOldChurch = "old church"
  show AmsterdamRoyalPalace = "royal palace"
  show AmsterdamOosterpark = "Oosterpark"
  show AmsterdamHortusBotanicus = "Hortus Botanicus"
  show AmsterdamRembrandtHouse = "Rembrandt House"
  show AmsterdamMuseum = "Amsterdam Museum"
  show AmsterdamHuisMarseille = "Huis Marseille"
  show AmsterdamFlowerMarket = "flower market"
  show AmsterdamRijksmuseum = "Rijksmuseum"
  show AmsterdamVondelpark = "Vondelpark"
  show AmsterdamHeinekenExperience = "Heineken Experience"
  show AmsterdamVanGoghMuseum = "Van Gogh Museum"

type Exit = (Direction, RoomName)

data Room = Room
  { rname :: RoomName,
    desc :: String,
    exits :: [Exit],
    objects :: [ItemName]
  }
  deriving (Show, Eq)

------------------------------
-- List of predefined Rooms --
------------------------------

-- Hyde Park

backyard :: Room
backyard =
  Room
    { rname = Backyard,
      desc = "You are in the backyard.",
      exits =
        [ (S, Kitchen)
        ],
      objects =
        [ Rake
        ]
    }

kitchen :: Room
kitchen =
  Room
    { rname = Kitchen,
      desc = "You are in the kitchen.",
      exits =
        [ (N, Backyard),
          (E, LivingRoom)
        ],
      objects =
        [ Knife,
          Fork,
          Pot
        ]
    }

livingRoom :: Room
livingRoom =
  Room
    { rname = LivingRoom,
      desc = "You are in the living room.",
      exits =
        [ (N, Bedroom),
          (S, Foyer),
          (E, Bathroom),
          (W, Kitchen)
        ],
      objects =
        [
        ]
    }

bedroom :: Room
bedroom =
  Room
    { rname = Bedroom,
      desc = "You are in the bedroom.",
      exits =
        [ (S, LivingRoom),
          (E, WalkInCloset)
        ],
      objects =
        [ Bed
        ]
    }

walkInCloset :: Room
walkInCloset =
  Room
    { rname = WalkInCloset,
      desc = "You are in the walk-in closet.",
      exits =
        [ (W, Bedroom)
        ],
      objects =
        [
        ]
    }

bathroom :: Room
bathroom =
  Room
    { rname = Bathroom,
      desc = "You are in the bathroom.",
      exits =
        [ (W, LivingRoom)
        ],
      objects =
        [
        ]
    }

foyer :: Room
foyer =
  Room
    { rname = Foyer,
      desc = "You are in the foyer.",
      exits =
        [ (N, LivingRoom),
          (S, FrontYard)
        ],
      objects =
        [
        ]
    }

frontYard :: Room
frontYard =
  Room
    { rname = FrontYard,
      desc = "You are in the front yard.",
      exits =
        [ (N, Foyer),
          (S, FiftyFourth),
          (W, YardTaxi)
        ],
      objects =
        [ LondonPlaneTicket
        ]
    }

yardTaxi :: Room
yardTaxi =
  Room
    { rname = YardTaxi,
      desc = "You are in front of a taxi.",
      exits =
        [ (E, FrontYard),
          (ORD, OrdLoadingZone)
        ],
      objects =
        [
        ]
    }

fiftyFourth :: Room
fiftyFourth =
  Room
    { rname = FiftyFourth,
      desc = "You are on 54th street.",
      exits =
        [ (N, FrontYard),
          (S, FiftyFifth)
        ],
      objects =
        [
        ]
    }

fiftyFifth :: Room
fiftyFifth =
  Room
    { rname = FiftyFifth,
      desc = "You are on 55th street.",
      exits =
        [ (N, FiftyFourth),
          (S, NorthResidenceHall)
        ],
      objects =
        [
        ]
    }

northResidenceHall :: Room
northResidenceHall =
  Room
    { rname = NorthResidenceHall,
      desc = "You are in front of North Residence Hall.",
      exits =
        [ (N, FiftyFifth),
          (S, MaxPEast),
          (W, Insomnia)
        ],
      objects =
        [
        ]
    }

insomnia :: Room
insomnia =
  Room
    { rname = Insomnia,
      desc = "You are in front of Insomnia Cookies.",
      exits =
        [ (S, BreakupForest),
          (E, NorthResidenceHall),
          (W, Ratner)
        ],
      objects =
        [
        ]
    }

ratner :: Room
ratner =
  Room
    { rname = Ratner,
      desc = "You are in front of Ratner Athletics Center.",
      exits =
        [ (S, MaxPWest),
          (E, Insomnia)
        ],
      objects =
        [
        ]
    }

maxPEast :: Room
maxPEast =
  Room
    { rname = MaxPEast,
      desc = "You are in front of Max Palevsky East Residence Hall.",
      exits =
        [ (N, NorthResidenceHall),
          (S, UniversityChurch),
          (W, MaxPCentral)
        ],
      objects =
        [
        ]
    }

breakupForest :: Room
breakupForest =
  Room
    { rname = BreakupForest,
      desc = "You are in breakup forest.",
      exits =
        [ (N, Insomnia),
          (S, MaxPCentral)
        ],
      objects =
        [
        ]
    }

maxPCentral :: Room
maxPCentral =
  Room
    { rname = MaxPCentral,
      desc = "You are in front of Max Palevsky Central Residence Hall.",
      exits =
        [ (N, BreakupForest),
          (S, Bartlett),
          (E, MaxPEast),
          (W, MaxPWest)
        ],
      objects =
        [
        ]
    }

maxPWest :: Room
maxPWest =
  Room
    { rname = MaxPWest,
      desc = "You are in front of Max Palevsky West Residence Hall.",
      exits =
        [ (N, Ratner),
          (S, Mansueto),
          (E, MaxPCentral)
        ],
      objects =
        [
        ]
    }

universityChurch :: Room
universityChurch =
  Room
    { rname = UniversityChurch,
      desc = "You are in front of university church.",
      exits =
        [ (N, MaxPEast),
          (W, Bartlett)
        ],
      objects =
        [
        ]
    }

bartlett :: Room
bartlett =
  Room
    { rname = Bartlett,
      desc = "You are in front of Bartlett Dining Commons.",
      exits =
        [ (N, MaxPCentral),
          (S, Reynolds),
          (E, UniversityChurch),
          (W, Regenstein)
        ],
      objects =
        [
        ]
    }

regenstein :: Room
regenstein =
  Room
    { rname = Regenstein,
      desc = "You are in front of Regenstein Library.",
      exits =
        [ (S, Quad),
          (E, Bartlett),
          (W, Mansueto)
        ],
      objects =
        [
        ]
    }

mansueto :: Room
mansueto =
  Room
    { rname = Mansueto,
      desc = "You are in front of Mansueto Library.",
      exits =
        [ (N, MaxPWest),
          (S, Bookstore),
          (E, Regenstein),
          (W, Kersten)
        ],
      objects =
        [
        ]
    }

kersten :: Room
kersten =
  Room
    { rname = Kersten,
      desc = "You are in front of Kersten Physics Teaching Center.",
      exits =
        [ (S, CrerarQuad),
          (E, Mansueto)
        ],
      objects =
        [
        ]
    }

reynolds :: Room
reynolds =
  Room
    { rname = Reynolds,
      desc = "You are in front of Reynolds Club.",
      exits =
        [ (N, Bartlett),
          (S, HutchQuad)
        ],
      objects =
        [
        ]
    }

hutchQuad :: Room
hutchQuad =
  Room
    { rname = HutchQuad,
      desc = "You are on Hutch quad.",
      exits =
        [ (N, Reynolds),
          (W, Quad)
        ],
      objects =
        [
        ]
    }

quad :: Room
quad =
  Room
    { rname = Quad,
      desc = "You are on the quad.",
      exits =
        [ (N, Regenstein),
          (S, HarperQuad),
          (E, HutchQuad),
          (W, Bookstore)
        ],
      objects =
        [
        ]
    }

bookstore :: Room
bookstore =
  Room
    { rname = Bookstore,
      desc = "You are in front of the bookstore.",
      exits =
        [ (N, Mansueto),
          (S, Cobb),
          (E, Quad),
          (W, CrerarQuad)
        ],
      objects =
        [
        ]
    }

crerarQuad :: Room
crerarQuad =
  Room
    { rname = CrerarQuad,
      desc = "You are on Crerar quad.",
      exits =
        [ (N, Kersten),
          (E, Bookstore)
        ],
      objects =
        [
        ]
    }

harperQuad :: Room
harperQuad =
  Room
    { rname = HarperQuad,
      desc = "You are on Harper quad.",
      exits =
        [ (N, Quad),
          (W, Cobb)
        ],
      objects =
        [
        ]
    }

cobb :: Room
cobb =
  Room
    { rname = Cobb,
      desc = "You are in front of Cobb Hall.",
      exits =
        [ (N, Bookstore),
          (E, HarperQuad)
        ],
      objects =
        [
        ]
    }

-- ORD

ordLoadingZone :: Room
ordLoadingZone =
  Room
    { rname = OrdLoadingZone,
      desc = "You are in the loading zone outside the airport.",
      exits =
        [ (N, OrdLobby),
          (HydePark, FrontYard)
        ],
      objects =
        [
        ]
    }

ordLobby :: Room
ordLobby =
  Room
    { rname = OrdLobby,
      desc = "You are in the airport lobby.",
      exits =
        [ (N, OrdSecurity),
          (S, OrdLoadingZone)
        ],
      objects =
        [
        ]
    }

ordSecurity :: Room
ordSecurity =
  Room
    { rname = OrdSecurity,
      desc = "You are in airport security.",
      exits =
        [ (N, OrdInternationalTerminal),
          (S, OrdLobby)
        ],
      objects =
        [
        ]
    }

ordInternationalTerminal :: Room
ordInternationalTerminal =
  Room
    { rname = OrdInternationalTerminal,
      desc = "You are in the international terminal.",
      exits =
        [ (S, OrdSecurity),
          (E, OrdCoffeeShop),
          (W, OrdBurgerRestaurant)
        ],
      objects =
        [
        ]
    }

ordBurgerRestaurant :: Room
ordBurgerRestaurant =
  Room
    { rname = OrdBurgerRestaurant,
      desc = "You are in a burger restaurant in the airport.",
      exits =
        [ (E, OrdInternationalTerminal),
          (W, OrdLounge)
        ],
      objects =
        [
        ]
    }

ordLounge :: Room
ordLounge =
  Room
    { rname = OrdLounge,
      desc = "You are in the airport lounge.",
      exits =
        [ (E, OrdBurgerRestaurant)
        ],
      objects =
        [
        ]
    }

ordCoffeeShop :: Room
ordCoffeeShop =
  Room
    { rname = OrdCoffeeShop,
      desc = "You are in a coffee shop in the airport.",
      exits =
        [ (E, OrdGate),
          (W, OrdInternationalTerminal)
        ],
      objects =
        [
        ]
    }

ordGate :: Room
ordGate =
  Room
    { rname = OrdGate,
      desc = "You are at the gate in the airport.",
      exits =
        [ (E, OrdPlane),
          (W, OrdCoffeeShop)
        ],
      objects =
        [
        ]
    }

ordPlane :: Room
ordPlane =
  Room
    { rname = OrdPlane,
      desc = "You are on an airplane.",
      exits =
        [ (W, OrdGate),
          (LHRfly, LhrGate)
        ],
      objects =
        [
        ]
    }

-- LHR

lhrPlane :: Room
lhrPlane =
  Room
    { rname = LhrPlane,
      desc = "You are on an airplane.",
      exits =
        [ (E, LhrGate),
          (LHRAMS, AmsGate)
        ],
      objects =
        [
        ]
    }

lhrGate :: Room
lhrGate =
  Room
    { rname = LhrGate,
      desc = "You are at the gate in the airport.",
      exits =
        [ (E, LhrCustoms),
          (W, LhrPlane)
        ],
      objects =
        [
        ]
    }

lhrCustoms :: Room
lhrCustoms =
  Room
    { rname = LhrCustoms,
      desc = "You are going through British customs at the airport.",
      exits =
        [ (E, LhrSecurity),
          (W, LhrGate)
        ],
      objects =
        [
        ]
    }

lhrSecurity :: Room
lhrSecurity =
  Room
    { rname = LhrSecurity,
      desc = "You are in airport security.",
      exits =
        [ (E, LhrLobby),
          (W, LhrCustoms)
        ],
      objects =
        [
        ]
    }

lhrLobby :: Room
lhrLobby =
  Room
    { rname = LhrLobby,
      desc = "You are in the airport lobby.",
      exits =
        [ (E, LhrAirportTubeStation),
          (W, LhrSecurity)
        ],
      objects =
        [
        ]
    }

lhrAirportTubeStation :: Room
lhrAirportTubeStation =
  Room
    { rname = LhrAirportTubeStation,
      desc = "You are in the airport tube station.",
      exits =
        [ (E, LhrTrain),
          (W, LhrLobby)
        ],
      objects =
        [ LondonTubeTicket
        ]
    }

lhrTrain :: Room
lhrTrain =
  Room
    { rname = LhrTrain,
      desc = "You are in a train.",
      exits =
        [ (W, LhrAirportTubeStation),
          (LondonDowntown, LonWestminsterTubeStation)
        ],
      objects =
        [
        ]
    }

-- London

lonWestminsterTubeStation :: Room
lonWestminsterTubeStation =
  Room
    { rname = LonWestminsterTubeStation,
      desc = "You are in the Westminster tube station.",
      exits =
        [ (N, LonTrafalgarSquare),
          (S, LonHousesOfParliament),
          (E, LonLondonEye),
          (W, LonWestminsterAbbey),
          (LHRreturn, LhrAirportTubeStation)
        ],
      objects =
        [
        ]
    }

lonWestminsterAbbey :: Room
lonWestminsterAbbey =
  Room
    { rname = LonWestminsterAbbey,
      desc = "You are in front of Westminster Abbey.",
      exits =
        [ (S, LonRoyalBotanicGardens),
          (E, LonWestminsterTubeStation)
        ],
      objects =
        [
        ]
    }

lonRoyalBotanicGardens :: Room
lonRoyalBotanicGardens =
  Room
    { rname = LonRoyalBotanicGardens,
      desc = "You are in the royal botanic gardens.",
      exits =
        [ (N, LonWestminsterAbbey),
          (E, LonHousesOfParliament)
        ],
      objects =
        [
        ]
    }

lonHousesOfParliament :: Room
lonHousesOfParliament =
  Room
    { rname = LonHousesOfParliament,
      desc = "You are in front of the houses of parliament.",
      exits =
        [ (N, LonWestminsterTubeStation),
          (W, LonRoyalBotanicGardens)
        ],
      objects =
        [
        ]
    }

lonLondonEye :: Room
lonLondonEye =
  Room
    { rname = LonLondonEye,
      desc = "You are in front of the London Eye.",
      exits =
        [ (E, LonShakespearesGlobeTheatre),
          (W, LonWestminsterTubeStation)
        ],
      objects =
        [
        ]
    }

lonShakespearesGlobeTheatre :: Room
lonShakespearesGlobeTheatre =
  Room
    { rname = LonShakespearesGlobeTheatre,
      desc = "You are in front of Shakespeare's Globe Theatre.",
      exits =
        [ (N, LonStPaulsCathedral),
          (E, LonLondonBridge),
          (W, LonLondonEye)
        ],
      objects =
        [
        ]
    }

lonLondonBridge :: Room
lonLondonBridge =
  Room
    { rname = LonLondonBridge,
      desc = "You are in front of London Bridge.",
      exits =
        [ (N, LonSkyGarden),
          (W, LonShakespearesGlobeTheatre)
        ],
      objects =
        [
        ]
    }

lonSkyGarden :: Room
lonSkyGarden =
  Room
    { rname = LonSkyGarden,
      desc = "You are in front of the sky garden.",
      exits =
        [ (S, LonLondonBridge),
          (W, LonStPaulsCathedral)
        ],
      objects =
        [
        ]
    }

lonStPaulsCathedral :: Room
lonStPaulsCathedral =
  Room
    { rname = LonStPaulsCathedral,
      desc = "You are in front of St Paul's Cathedral.",
      exits =
        [ (S, LonShakespearesGlobeTheatre),
          (E, LonSkyGarden),
          (W, LonFilmMuseum)
        ],
      objects =
        [
        ]
    }

lonFilmMuseum :: Room
lonFilmMuseum =
  Room
    { rname = LonFilmMuseum,
      desc = "You are in front of the film museum.",
      exits =
        [ (E, LonStPaulsCathedral),
          (W, LonTrafalgarSquare)
        ],
      objects =
        [
        ]
    }

lonTrafalgarSquare :: Room
lonTrafalgarSquare =
  Room
    { rname = LonTrafalgarSquare,
      desc = "You are in Trafalgar Square.",
      exits =
        [ (N, LonZoo),
          (S, LonWestminsterTubeStation),
          (E, LonFilmMuseum),
          (W, LonMadameTussauds)
        ],
      objects =
        [ LondonReturnTubeTicket
        ]
    }

lonZoo :: Room
lonZoo =
  Room
    { rname = LonZoo,
      desc = "You are at the zoo.",
      exits =
        [ (S, LonTrafalgarSquare),
          (W, LonRegentsPark)
        ],
      objects =
        [
        ]
    }

lonRegentsPark :: Room
lonRegentsPark =
  Room
    { rname = LonRegentsPark,
      desc = "You are in Regent's Park.",
      exits =
        [ (S, LonMadameTussauds),
          (E, LonZoo)
        ],
      objects =
        [
        ]
    }

lonMadameTussauds :: Room
lonMadameTussauds =
  Room
    { rname = LonMadameTussauds,
      desc = "You are in front of Madame Tussauds wax museum.",
      exits =
        [ (N, LonRegentsPark),
          (E, LonTrafalgarSquare)
        ],
      objects =
        [ AmsterdamPlaneTicket
        ]
    }

-- AMS
  
amsPlane :: Room
amsPlane =
  Room
    { rname = AmsPlane,
      desc = "You are on an airplane.",
      exits =
        [ (S, AmsGate)
        ],
      objects =
        [
        ]
    }

amsGate :: Room
amsGate =
  Room
    { rname = AmsGate,
      desc = "You are at the gate in the airport.",
      exits =
        [ (N, AmsPlane),
          (S, AmsCustoms)
        ],
      objects =
        [
        ]
    }

amsCustoms :: Room
amsCustoms =
  Room
    { rname = AmsCustoms,
      desc = "You are going through Dutch customs at the airport.",
      exits =
        [ (N, AmsGate),
          (S, AmsSecurity)
        ],
      objects =
        [
        ]
    }

amsSecurity :: Room
amsSecurity =
  Room
    { rname = AmsSecurity,
      desc = "You are in airport security.",
      exits =
        [ (N, AmsCustoms),
          (S, AmsLobby)
        ],
      objects =
        [
        ]
    }

amsLobby :: Room
amsLobby =
  Room
    { rname = AmsLobby,
      desc = "You are in the airport lobby.",
      exits =
        [ (N, AmsSecurity),
          (S, AmsAirportStation)
        ],
      objects =
        [
        ]
    }

amsAirportStation :: Room
amsAirportStation =
  Room
    { rname = AmsAirportStation,
      desc = "You are in the airport train station.",
      exits =
        [ (N, AmsLobby),
          (S, AmsTrain)
        ],
      objects =
        [ 
        ]
    }

amsTrain :: Room
amsTrain =
  Room
    { rname = AmsTrain,
      desc = "You are in a train.",
      exits =
        [ (N, AmsAirportStation)
        ],
      objects =
        [
        ]
    }

-- Amsterdam

amsterdamCentraalStation :: Room
amsterdamCentraalStation =
  Room
    { rname = AmsterdamCentraalStation,
      desc = "You are in Centraal Station.",
      exits =
        [ (S, AmsterdamOldChurch),
          (E, AmsterdamNemoScienceMuseum),
          (W, AmsterdamAnneFrankHouse)
        ],
      objects =
        [
        ]
    }

amsterdamNemoScienceMuseum :: Room
amsterdamNemoScienceMuseum =
  Room
    { rname = AmsterdamNemoScienceMuseum,
      desc = "You are in front of the Nemo Science Museum.",
      exits =
        [ (W, AmsterdamCentraalStation)
        ],
      objects =
        [
        ]
    }

amsterdamAnneFrankHouse :: Room
amsterdamAnneFrankHouse =
  Room
    { rname = AmsterdamAnneFrankHouse,
      desc = "You are in front of the Anne Frank House.",
      exits =
        [ (S, AmsterdamRoyalPalace),
          (E, AmsterdamCentraalStation)
        ],
      objects =
        [
        ]
    }

amsterdamOldChurch :: Room
amsterdamOldChurch =
  Room
    { rname = AmsterdamOldChurch,
      desc = "You are in front of an old church.",
      exits =
        [ (N, AmsterdamCentraalStation),
          (S, AmsterdamRembrandtHouse),
          (W, AmsterdamRoyalPalace)
        ],
      objects =
        [
        ]
    }

amsterdamRoyalPalace :: Room
amsterdamRoyalPalace =
  Room
    { rname = AmsterdamRoyalPalace,
      desc = "You are in front of the royal palace.",
      exits =
        [ (N, AmsterdamAnneFrankHouse),
          (S, AmsterdamMuseum),
          (E, AmsterdamOldChurch)
        ],
      objects =
        [
        ]
    }

amsterdamOosterpark :: Room
amsterdamOosterpark =
  Room
    { rname = AmsterdamOosterpark,
      desc = "You are in Oosterpark.",
      exits =
        [ (W, AmsterdamHortusBotanicus)
        ],
      objects =
        [
        ]
    }

amsterdamHortusBotanicus :: Room
amsterdamHortusBotanicus =
  Room
    { rname = AmsterdamHortusBotanicus,
      desc = "You are in front of the Hortus Botanicus botanical garden.",
      exits =
        [ (E, AmsterdamOosterpark),
          (W, AmsterdamRembrandtHouse)
        ],
      objects =
        [
        ]
    }

amsterdamRembrandtHouse :: Room
amsterdamRembrandtHouse =
  Room
    { rname = AmsterdamRembrandtHouse,
      desc = "You are in front of the Rembrandt House.",
      exits =
        [ (N, AmsterdamOldChurch),
          (E, AmsterdamHortusBotanicus),
          (W, AmsterdamMuseum)
        ],
      objects =
        [
        ]
    }

amsterdamMuseum :: Room
amsterdamMuseum =
  Room
    { rname = AmsterdamMuseum,
      desc = "You are in front of the Amsterdam Museum.",
      exits =
        [ (N, AmsterdamRoyalPalace),
          (S, AmsterdamFlowerMarket),
          (E, AmsterdamRembrandtHouse),
          (W, AmsterdamHuisMarseille)
        ],
      objects =
        [
        ]
    }

amsterdamHuisMarseille :: Room
amsterdamHuisMarseille =
  Room
    { rname = AmsterdamHuisMarseille,
      desc = "You are in front of the Huis Marseille photography museum.",
      exits =
        [ (E, AmsterdamMuseum)
        ],
      objects =
        [
        ]
    }

amsterdamFlowerMarket :: Room
amsterdamFlowerMarket =
  Room
    { rname = AmsterdamFlowerMarket,
      desc = "You are in front of the flower market.",
      exits =
        [ (N, AmsterdamMuseum),
          (S, AmsterdamHeinekenExperience),
          (W, AmsterdamRijksmuseum)
        ],
      objects =
        [
        ]
    }

amsterdamRijksmuseum :: Room
amsterdamRijksmuseum =
  Room
    { rname = AmsterdamRijksmuseum,
      desc = "You are in front of the Rijksmuseum.",
      exits =
        [ (S, AmsterdamVanGoghMuseum),
          (E, AmsterdamFlowerMarket),
          (W, AmsterdamVondelpark)
        ],
      objects =
        [
        ]
    }

amsterdamVondelpark :: Room
amsterdamVondelpark =
  Room
    { rname = AmsterdamVondelpark,
      desc = "You are in Vondelpark.",
      exits =
        [ (E, AmsterdamRijksmuseum)
        ],
      objects =
        [
        ]
    }

amsterdamHeinekenExperience :: Room
amsterdamHeinekenExperience =
  Room
    { rname = AmsterdamHeinekenExperience,
      desc = "You are in front of the Heineken Experience brewery.",
      exits =
        [ (N, AmsterdamFlowerMarket)
        ],
      objects =
        [
        ]
    }

amsterdamVanGoghMuseum :: Room
amsterdamVanGoghMuseum =
  Room
    { rname = AmsterdamVanGoghMuseum,
      desc = "You are in front of the Van Gogh Museum.",
      exits =
        [ (N, AmsterdamRijksmuseum)
        ],
      objects =
        [
        ]
    }

-- Room Names
roomNames :: [RoomName]
roomNames = map rname rooms

rooms :: [Room]
rooms =
  [ -- Hyde Park
    backyard,
    kitchen,
    livingRoom,
    bedroom,
    walkInCloset,
    bathroom,
    foyer,
    frontYard,
    yardTaxi,
    fiftyFourth,
    fiftyFifth,
    northResidenceHall,
    insomnia,
    ratner,
    maxPEast,
    breakupForest,
    maxPCentral,
    maxPWest,
    universityChurch,
    bartlett,
    regenstein,
    mansueto,
    kersten,
    reynolds,
    hutchQuad,
    quad,
    bookstore,
    crerarQuad,
    harperQuad,
    cobb,
    
    -- ORD
    ordLoadingZone,
    ordLobby,
    ordSecurity,
    ordInternationalTerminal,
    ordBurgerRestaurant,
    ordLounge,
    ordCoffeeShop,
    ordGate,
    ordPlane,

    -- LHR
    lhrPlane,
    lhrGate,
    lhrCustoms,
    lhrSecurity,
    lhrLobby,
    lhrAirportTubeStation,
    lhrTrain,

    -- London
    lonWestminsterTubeStation,
    lonWestminsterAbbey,
    lonRoyalBotanicGardens,
    lonHousesOfParliament,
    lonLondonEye,
    lonShakespearesGlobeTheatre,
    lonLondonBridge,
    lonSkyGarden,
    lonStPaulsCathedral,
    lonFilmMuseum,
    lonTrafalgarSquare,
    lonZoo,
    lonRegentsPark,
    lonMadameTussauds,

    -- AMS
    amsPlane,
    amsGate,
    amsCustoms,
    amsSecurity,
    amsLobby,
    amsAirportStation,
    amsTrain,

    -- Amsterdam
    amsterdamCentraalStation,
    amsterdamNemoScienceMuseum,
    amsterdamAnneFrankHouse,
    amsterdamOldChurch,
    amsterdamRoyalPalace,
    amsterdamOosterpark,
    amsterdamHortusBotanicus,
    amsterdamRembrandtHouse,
    amsterdamMuseum,
    amsterdamHuisMarseille,
    amsterdamFlowerMarket,
    amsterdamRijksmuseum,
    amsterdamVondelpark,
    amsterdamHeinekenExperience,
    amsterdamVanGoghMuseum
  ]

addItem :: ItemName -> Room -> Room
addItem i r =
  Room
  {
    rname = rname r,
    desc = desc r,
    exits = exits r,
    objects = i : objects r
  }

removeItem :: ItemName -> Room -> Room
removeItem i r =
  Room
  {
    rname = rname r,
    desc = desc r,
    exits = exits r,
    objects = Data.List.delete i (objects r)
  }

hasObjects :: Room -> Bool
hasObjects r = not $ null (objects r)

normalExits :: Room -> [Exit]
normalExits r = normalExitsHelper (exits r) where
  normalExitsHelper xs =
    case xs of
      [] -> []
      e : es ->
        if elem (fst e) [N, S, E, W]
          then e : normalExitsHelper es
          else normalExitsHelper es