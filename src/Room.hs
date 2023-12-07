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

  -- CDG
  | CdgPlane
  | CdgGate
  | CdgCustoms
  | CdgSecurity
  | CdgLobby
  | CdgAirportStation
  | CdgTrain

  -- Paris
  | ParGareDuNord
  | ParSacreCoeur
  | ParMoulinRouge
  | ParPalaisGarnier
  | ParPalaisRoyal
  | ParLouvre
  | ParPlaceDeLaConcorde
  | ParArcDeTriomphe
  | ParPlaceDesVosges
  | ParNotreDame
  | ParSainteChapelle
  | ParHotelDesInvalides
  | ParEiffelTower
  | ParPantheon
  | ParMontparnasseTower
  | ParCatacombs

  -- PRG
  | PrgPlane
  | PrgGate
  | PrgCustoms
  | PrgSecurity
  | PrgLobby
  | PrgAirportStation
  | PrgTrain

  -- Prague
  | PraMainStation
  | PraSpanishSynagogue
  | PraOldJewishCemetary
  | PraPowderGate
  | PraTynChurch
  | PraHouseOfArts
  | PraNationalMuseum
  | PraAstronomicalClock
  | PraCharlesBridge
  | PraChurchOfStNicholas
  | PraPragueCastle
  | PraNationalTheatre
  | PraLennonWall
  | PraPetrinTower
  | PraStrahovMonastery
  | PraDancingHouse

  -- IST
  | IstPlane
  | IstGate
  | IstCustoms
  | IstSecurity
  | IstLobby
  | IstLoadingZone
  | IstTaxi

  -- Istanbul
  | IstanbulTaxi
  | IstanbulSerpentineColumn
  | IstanbulSpiceBazaar
  | IstanbulGrandBazaar
  | IstanbulHippodromeSquare
  | IstanbulObeliskOfTheodesius
  | IstanbulIstiklalAvenue
  | IstanbulTurkishAndIslamicArtMuseum
  | IstanbulBlueMosque
  | IstanbulBogaziciUniversity
  | IstanbulTaksimSquare
  | IstanbulGalataTower
  | IstanbulTopkapiPalace
  | IstanbulHagiaSophia
  | IstanbulOrtakoySquare
  | IstanbulDolmabahcePalace
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
  show MaxPWest = "Max Palevsky West"
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

  -- CDG
  show CdgPlane = "CDG Plane"
  show CdgGate = "CDG Gate"
  show CdgCustoms = "CDG Customs"
  show CdgSecurity = "CDG Security"
  show CdgLobby = "CDG Lobby"
  show CdgAirportStation = "CDG Airport Station"
  show CdgTrain = "CDG Train"

  -- Paris
  show ParGareDuNord = "Gare du Nord station"
  show ParSacreCoeur = "Sacré Cœur"
  show ParMoulinRouge = "Moulin Rouge"
  show ParPalaisGarnier = "Palais Garnier"
  show ParPalaisRoyal = "Palais Royal"
  show ParLouvre = "Louvre"
  show ParPlaceDeLaConcorde = "Place de la Concorde"
  show ParArcDeTriomphe = "Arc de Triomphe"
  show ParPlaceDesVosges = "Place des Vosges"
  show ParNotreDame = "Notre Dame"
  show ParSainteChapelle = "Sainte-Chapelle"
  show ParHotelDesInvalides = "Hôtel des Invalides"
  show ParEiffelTower = "Eiffel Tower"
  show ParPantheon = "Pantheon"
  show ParMontparnasseTower = "Montparnasse Tower"
  show ParCatacombs = "catacombs"

  -- PRG
  show PrgPlane = "PRG Plane"
  show PrgGate = "PRG Gate"
  show PrgCustoms = "PRG Customs"
  show PrgSecurity = "PRG Security"
  show PrgLobby = "PRG Lobby"
  show PrgAirportStation = "PRG Airport Station"
  show PrgTrain = "PRG Train"

  -- Prague
  show PraMainStation = "Prague main station"
  show PraSpanishSynagogue = "Spanish Synagogue"
  show PraOldJewishCemetary = "old Jewish cemetary"
  show PraPowderGate = "powder gate"
  show PraTynChurch = "Týn Church"
  show PraHouseOfArts = "house of arts"
  show PraNationalMuseum = "Prague national museum"
  show PraAstronomicalClock = "astronomical clock"
  show PraCharlesBridge = "Charles Bridge"
  show PraChurchOfStNicholas = "Church of St Nicholas"
  show PraPragueCastle = "Prague Castle"
  show PraNationalTheatre = "Prague national theatre"
  show PraLennonWall = "Lennon Wall"
  show PraPetrinTower = "Petřín Tower"
  show PraStrahovMonastery = "Strahov Monastery"
  show PraDancingHouse = "dancing house"

  -- IST
  show IstPlane = "IST Plane"
  show IstGate = "IST Gate"
  show IstCustoms = "IST Customs"
  show IstSecurity = "IST Security"
  show IstLobby = "IST Lobby"
  show IstLoadingZone = "IST Loading Zone"
  show IstTaxi = "IST Taxi"

  -- Istanbul
  show IstanbulTaxi = "Istanbul taxi"
  show IstanbulSerpentineColumn = "Serpentine Column"
  show IstanbulSpiceBazaar = "Spice Bazaar"
  show IstanbulGrandBazaar = "Grand Bazaar"
  show IstanbulHippodromeSquare = "Hippodrome Square"
  show IstanbulObeliskOfTheodesius = "Obelisk of Theodesius"
  show IstanbulIstiklalAvenue = "İstiklal Avenue"
  show IstanbulTurkishAndIslamicArtMuseum = "Turkish and Islamic Art Museum"
  show IstanbulBlueMosque = "Blue Mosque"
  show IstanbulBogaziciUniversity = "Boğaziçi University"
  show IstanbulTaksimSquare = "Taksim Square"
  show IstanbulGalataTower = "Galata Tower"
  show IstanbulTopkapiPalace = "Topkapı Palace"
  show IstanbulHagiaSophia = "Hagia Sophia"
  show IstanbulOrtakoySquare = "Ortaköy Square"
  show IstanbulDolmabahcePalace = "Dolmabahçe Palace"

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
        [ Lamp
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
        [ TShirt,
          Jeans,
          Suit
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
        [ ToiletPaper
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
        [
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
        [ Cookie
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
        [ Dumbbell
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
        [ CafeteriaFood
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
        [ SmallBook,
          BigBook
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
        [ Bike
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
        [ Burger
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
        [ Coffee
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
        [ Rose,
          Azalea,
          LondonReturnTubeTicket
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
        [ TeaSet
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
        [ ShrekDVD
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
        [ Trash
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
        [ WaterBottle
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
        [ Tulip,
          Daffodil
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
        [ AmsterdamPlaneTicket,
          Wax
        ]
    }

-- AMS
  
amsPlane :: Room
amsPlane =
  Room
    { rname = AmsPlane,
      desc = "You are on an airplane.",
      exits =
        [ (S, AmsGate),
          (AMSCDG, CdgGate)
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
        [ AmsterdamTrainTicket
        ]
    }

amsTrain :: Room
amsTrain =
  Room
    { rname = AmsTrain,
      desc = "You are in a train.",
      exits =
        [ (N, AmsAirportStation),
          (AmsterdamDowntown, AmsterdamCentraalStation)
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
          (W, AmsterdamAnneFrankHouse),
          (AMSreturn, AmsAirportStation)
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
        [ AmsterdamReturnTrainTicket
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
        [ Bush,
          Leaf
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
        [ BigPainting
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
        [ Camera
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
        [ Stroopwafel
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
        [ Statue
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
        [ ParisPlaneTicket
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
        [ Beer
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
        [ SmallPainting
        ]
    }

-- CDG
  
cdgPlane :: Room
cdgPlane =
  Room
    { rname = CdgPlane,
      desc = "You are on an airplane.",
      exits =
        [ (W, CdgGate),
          (CDGPRG, PrgGate)
        ],
      objects =
        [
        ]
    }

cdgGate :: Room
cdgGate =
  Room
    { rname = CdgGate,
      desc = "You are at the gate in the airport.",
      exits =
        [ (E, CdgPlane),
          (W, CdgCustoms)
        ],
      objects =
        [
        ]
    }

cdgCustoms :: Room
cdgCustoms =
  Room
    { rname = CdgCustoms,
      desc = "You are going through French customs at the airport.",
      exits =
        [ (E, CdgGate),
          (W, CdgSecurity)
        ],
      objects =
        [
        ]
    }

cdgSecurity :: Room
cdgSecurity =
  Room
    { rname = CdgSecurity,
      desc = "You are in airport security.",
      exits =
        [ (E, CdgCustoms),
          (W, CdgLobby)
        ],
      objects =
        [
        ]
    }

cdgLobby :: Room
cdgLobby =
  Room
    { rname = CdgLobby,
      desc = "You are in the airport lobby.",
      exits =
        [ (E, CdgSecurity),
          (W, CdgAirportStation)
        ],
      objects =
        [
        ]
    }

cdgAirportStation :: Room
cdgAirportStation =
  Room
    { rname = CdgAirportStation,
      desc = "You are in the airport train station.",
      exits =
        [ (E, CdgLobby),
          (W, CdgTrain)
        ],
      objects =
        [ ParisTrainTicket
        ]
    }

cdgTrain :: Room
cdgTrain =
  Room
    { rname = CdgTrain,
      desc = "You are in a train.",
      exits =
        [ (E, CdgAirportStation),
          (ParisDowntown, ParGareDuNord)
        ],
      objects =
        [
        ]
    }

-- Paris

parGareDuNord :: Room
parGareDuNord =
  Room
    { rname = ParGareDuNord,
      desc = "You are in the Gare du Nord train station.",
      exits =
        [ (N, ParSacreCoeur),
          (S, ParLouvre),
          (W, ParPalaisGarnier),
          (CDGreturn, CdgAirportStation)
        ],
      objects =
        [
        ]
    }

parSacreCoeur :: Room
parSacreCoeur =
  Room
    { rname = ParSacreCoeur,
      desc = "You are in front of the Basilica of Sacré Cœur.",
      exits =
        [ (S, ParGareDuNord)
        ],
      objects =
        [
        ]
    }

parMoulinRouge :: Room
parMoulinRouge =
  Room
    { rname = ParMoulinRouge,
      desc = "You are in front of the Moulin Rouge cabaret.",
      exits =
        [ (S, ParPalaisGarnier)
        ],
      objects =
        [ Croissant
        ]
    }

parPalaisGarnier :: Room
parPalaisGarnier =
  Room
    { rname = ParPalaisGarnier,
      desc = "You are in front of the Palais Garnier opera house.",
      exits =
        [ (N, ParMoulinRouge),
          (S, ParPlaceDeLaConcorde),
          (E, ParGareDuNord)
        ],
      objects =
        [
        ]
    }

parPalaisRoyal :: Room
parPalaisRoyal =
  Room
    { rname = ParPalaisRoyal,
      desc = "You are in front of the Palais-Royal.",
      exits =
        [ (S, ParSainteChapelle),
          (W, ParLouvre)
        ],
      objects =
        [
        ]
    }

parLouvre :: Room
parLouvre =
  Room
    { rname = ParLouvre,
      desc = "You are in front of the Louvre museum.",
      exits =
        [ (N, ParGareDuNord),
          (E, ParPalaisRoyal),
          (W, ParPlaceDeLaConcorde)
        ],
      objects =
        [ MonaLisa
        ]
    }

parPlaceDeLaConcorde :: Room
parPlaceDeLaConcorde =
  Room
    { rname = ParPlaceDeLaConcorde,
      desc = "You are in Place de la Concorde.",
      exits =
        [ (N, ParPalaisGarnier),
          (S, ParHotelDesInvalides),
          (E, ParLouvre),
          (W, ParArcDeTriomphe)
        ],
      objects =
        [
        ]
    }

parArcDeTriomphe :: Room
parArcDeTriomphe =
  Room
    { rname = ParArcDeTriomphe,
      desc = "You are in front of the Arc de Triomphe.",
      exits =
        [ (S, ParEiffelTower),
          (E, ParPlaceDeLaConcorde)
        ],
      objects =
        [
        ]
    }

parPlaceDesVosges :: Room
parPlaceDesVosges =
  Room
    { rname = ParPlaceDesVosges,
      desc = "You are in Place des Vosges.",
      exits =
        [ (W, ParNotreDame)
        ],
      objects =
        [ ParisReturnTrainTicket
        ]
    }

parNotreDame :: Room
parNotreDame =
  Room
    { rname = ParNotreDame,
      desc = "You are in front of Notre-Dame de Paris cathedral.",
      exits =
        [ (E, ParPlaceDesVosges),
          (W, ParSainteChapelle)
        ],
      objects =
        [
        ]
    }

parSainteChapelle :: Room
parSainteChapelle =
  Room
    { rname = ParSainteChapelle,
      desc = "You are in front of the Sainte-Chapelle.",
      exits =
        [ (N, ParPalaisRoyal),
          (S, ParPantheon),
          (E, ParNotreDame)
        ],
      objects =
        [
        ]
    }

parHotelDesInvalides :: Room
parHotelDesInvalides =
  Room
    { rname = ParHotelDesInvalides,
      desc = "You are at the Hôtel des Invalides.",
      exits =
        [ (N, ParPlaceDeLaConcorde),
          (S, ParMontparnasseTower),
          (W, ParEiffelTower)
        ],
      objects =
        [
        ]
    }

parEiffelTower :: Room
parEiffelTower =
  Room
    { rname = ParEiffelTower,
      desc = "You are in front of the Eiffel Tower.",
      exits =
        [ (N, ParArcDeTriomphe),
          (E, ParHotelDesInvalides)
        ],
      objects =
        [
        ]
    }

parPantheon :: Room
parPantheon =
  Room
    { rname = ParPantheon,
      desc = "You are in front of the Pantheon.",
      exits =
        [ (N, ParSainteChapelle)
        ],
      objects =
        [
        ]
    }

parMontparnasseTower :: Room
parMontparnasseTower =
  Room
    { rname = ParMontparnasseTower,
      desc = "You are in front of Montparnasse Tower.",
      exits =
        [ (N, ParHotelDesInvalides),
          (S, ParCatacombs)
        ],
      objects =
        [
        ]
    }

parCatacombs :: Room
parCatacombs =
  Room
    { rname = ParCatacombs,
      desc = "You are in the catacombs.",
      exits =
        [ (N, ParMontparnasseTower)
        ],
      objects =
        [ PraguePlaneTicket
        ]
    }

-- PRG
  
prgPlane :: Room
prgPlane =
  Room
    { rname = PrgPlane,
      desc = "You are on an airplane.",
      exits =
        [ (S, PrgGate),
          (PRGIST, IstGate)
        ],
      objects =
        [
        ]
    }

prgGate :: Room
prgGate =
  Room
    { rname = PrgGate,
      desc = "You are at the gate in the airport.",
      exits =
        [ (N, PrgPlane),
          (S, PrgCustoms)
        ],
      objects =
        [
        ]
    }

prgCustoms :: Room
prgCustoms =
  Room
    { rname = PrgCustoms,
      desc = "You are going through Czech customs at the airport.",
      exits =
        [ (N, PrgGate),
          (S, PrgSecurity)
        ],
      objects =
        [
        ]
    }

prgSecurity :: Room
prgSecurity =
  Room
    { rname = PrgSecurity,
      desc = "You are in airport security.",
      exits =
        [ (N, PrgCustoms),
          (S, PrgLobby)
        ],
      objects =
        [
        ]
    }

prgLobby :: Room
prgLobby =
  Room
    { rname = PrgLobby,
      desc = "You are in the airport lobby.",
      exits =
        [ (N, PrgSecurity),
          (S, PrgAirportStation)
        ],
      objects =
        [
        ]
    }

prgAirportStation :: Room
prgAirportStation =
  Room
    { rname = PrgAirportStation,
      desc = "You are in the airport train station.",
      exits =
        [ (N, PrgLobby),
          (S, PrgTrain)
        ],
      objects =
        [ PragueTrainTicket
        ]
    }

prgTrain :: Room
prgTrain =
  Room
    { rname = PrgTrain,
      desc = "You are in a train.",
      exits =
        [ (N, PrgAirportStation),
          (PragueDowntown, PraMainStation)
        ],
      objects =
        [
        ]
    }

-- Prague

praMainStation :: Room
praMainStation =
  Room
    { rname = PraMainStation,
      desc = "You are in the main train station.",
      exits =
        [ (S, PraNationalMuseum),
          (W, PraPowderGate),
          (PRGreturn, PrgAirportStation)
        ],
      objects =
        [
        ]
    }

praSpanishSynagogue :: Room
praSpanishSynagogue =
  Room
    { rname = PraSpanishSynagogue,
      desc = "You are in front of the Spanish Synagogue.",
      exits =
        [ (S, PraPowderGate),
          (W, PraOldJewishCemetary)
        ],
      objects =
        [
        ]
    }

praOldJewishCemetary :: Room
praOldJewishCemetary =
  Room
    { rname = PraOldJewishCemetary,
      desc = "You are in the old Jewish cemetary.",
      exits =
        [ (S, PraTynChurch),
          (E, PraSpanishSynagogue)
        ],
      objects =
        [ RedGarnet,
          Spiderweb
        ]
    }

praPowderGate :: Room
praPowderGate =
  Room
    { rname = PraPowderGate,
      desc = "You are in front of Powder Gate.",
      exits =
        [ (N, PraSpanishSynagogue),
          (E, PraMainStation),
          (W, PraTynChurch)
        ],
      objects =
        [
        ]
    }

praTynChurch :: Room
praTynChurch =
  Room
    { rname = PraTynChurch,
      desc = "You are in front of Týn Church.",
      exits =
        [ (N, PraOldJewishCemetary),
          (S, PraAstronomicalClock),
          (E, PraPowderGate),
          (W, PraHouseOfArts)
        ],
      objects =
        [
        ]
    }

praHouseOfArts :: Room
praHouseOfArts =
  Room
    { rname = PraHouseOfArts,
      desc = "You are in front of the house of arts.",
      exits =
        [ (E, PraTynChurch)
        ],
      objects =
        [
        ]
    }

praNationalMuseum :: Room
praNationalMuseum =
  Room
    { rname = PraNationalMuseum,
      desc = "You are in front of the national museum.",
      exits =
        [ (N, PraMainStation)
        ],
      objects =
        [
        ]
    }

praAstronomicalClock :: Room
praAstronomicalClock =
  Room
    { rname = PraAstronomicalClock,
      desc = "You are in front of the astronomical clock.",
      exits =
        [ (N, PraTynChurch),
          (S, PraNationalTheatre),
          (W, PraCharlesBridge)
        ],
      objects =
        [
        ]
    }

praCharlesBridge :: Room
praCharlesBridge =
  Room
    { rname = PraCharlesBridge,
      desc = "You are on Charles Bridge.",
      exits =
        [ (E, PraAstronomicalClock),
          (W, PraChurchOfStNicholas)
        ],
      objects =
        [
        ]
    }

praChurchOfStNicholas :: Room
praChurchOfStNicholas =
  Room
    { rname = PraChurchOfStNicholas,
      desc = "You are in front of the Church of St Nicholas.",
      exits =
        [ (S, PraLennonWall),
          (E, PraCharlesBridge),
          (W, PraPragueCastle)
        ],
      objects =
        [
        ]
    }

praPragueCastle :: Room
praPragueCastle =
  Room
    { rname = PraPragueCastle,
      desc = "You are in front of Prague Castle.",
      exits =
        [ (S, PraPetrinTower),
          (E, PraChurchOfStNicholas)
        ],
      objects =
        [ Stick
        ]
    }

praNationalTheatre :: Room
praNationalTheatre =
  Room
    { rname = PraNationalTheatre,
      desc = "You are in front of the national theatre.",
      exits =
        [ (N, PraAstronomicalClock),
          (S, PraDancingHouse)
        ],
      objects =
        [ Flyer
        ]
    }

praLennonWall :: Room
praLennonWall =
  Room
    { rname = PraLennonWall,
      desc = "You are in front of Lennon Wall.",
      exits =
        [ (N, PraChurchOfStNicholas),
          (W, PraPetrinTower)
        ],
      objects =
        [
        ]
    }

praPetrinTower :: Room
praPetrinTower =
  Room
    { rname = PraPetrinTower,
      desc = "You are in front of Petřín Tower.",
      exits =
        [ (N, PraPragueCastle),
          (E, PraLennonWall),
          (W, PraStrahovMonastery)
        ],
      objects =
        [ PicnicBlanket,
          PicnicBasket,
          Apple,
          Orange,
          Banana,
          RoastBeefSandwich
        ]
    }

praStrahovMonastery :: Room
praStrahovMonastery =
  Room
    { rname = PraStrahovMonastery,
      desc = "You are in front of Strahov Monastery.",
      exits =
        [ (E, PraPetrinTower)
        ],
      objects =
        [ IstanbulPlaneTicket
        ]
    }

praDancingHouse :: Room
praDancingHouse =
  Room
    { rname = PraDancingHouse,
      desc = "You are in front of dancing house.",
      exits =
        [ (N, PraNationalTheatre)
        ],
      objects =
        [ PragueReturnTrainTicket
        ]
    }

-- IST

istPlane :: Room
istPlane =
  Room
    { rname = IstPlane,
      desc = "You are on an airplane.",
      exits =
        [ (N, IstGate),
          (ISTORD, OrdGate)
        ],
      objects =
        [
        ]
    }

istGate :: Room
istGate =
  Room
    { rname = IstGate,
      desc = "You are at the gate in the airport.",
      exits =
        [ (N, IstCustoms),
          (S, IstPlane)
        ],
      objects =
        [
        ]
    }

istCustoms :: Room
istCustoms =
  Room
    { rname = IstCustoms,
      desc = "You are going through Turkish customs at the airport.",
      exits =
        [ (N, IstSecurity),
          (S, IstGate)
        ],
      objects =
        [
        ]
    }

istSecurity :: Room
istSecurity =
  Room
    { rname = IstSecurity,
      desc = "You are in airport security.",
      exits =
        [ (N, IstLobby),
          (S, IstCustoms)
        ],
      objects =
        [
        ]
    }

istLobby :: Room
istLobby =
  Room
    { rname = IstLobby,
      desc = "You are in the airport lobby.",
      exits =
        [ (N, IstLoadingZone),
          (S, IstSecurity)
        ],
      objects =
        [
        ]
    }

istLoadingZone :: Room
istLoadingZone =
  Room
    { rname = IstLoadingZone,
      desc = "You are in the loading zone outside the airport.",
      exits =
        [ (N, IstTaxi),
          (S, IstLobby)
        ],
      objects =
        [
        ]
    }

istTaxi :: Room
istTaxi =
  Room
    { rname = IstTaxi,
      desc = "You are in front of a taxi.",
      exits =
        [ (S, IstLoadingZone),
          (IstanbulDowntown, IstanbulTaxi)
        ],
      objects =
        [
        ]
    }

-- Istanbul

istanbulTaxi :: Room
istanbulTaxi =
  Room
    { rname = IstanbulTaxi,
      desc = "You are in front of a taxi.",
      exits =
        [ (E, IstanbulGrandBazaar),
          (ISTreturn, IstLoadingZone)
        ],
      objects =
        [
        ]
    }

istanbulSerpentineColumn :: Room
istanbulSerpentineColumn =
  Room
    { rname = IstanbulSerpentineColumn,
      desc = "You are in front of the Serpentine Column.",
      exits =
        [ (E, IstanbulHippodromeSquare)
        ],
      objects =
        [
        ]
    }

istanbulSpiceBazaar :: Room
istanbulSpiceBazaar =
  Room
    { rname = IstanbulSpiceBazaar,
      desc = "You are at the spice bazaar.",
      exits =
        [ (S, IstanbulGrandBazaar)
        ],
      objects =
        [ Spices,
          BlackTea
        ]
    }

istanbulGrandBazaar :: Room
istanbulGrandBazaar =
  Room
    { rname = IstanbulGrandBazaar,
      desc = "You are at the Grand Bazaar.",
      exits =
        [ (N, IstanbulSpiceBazaar),
          (S, IstanbulHippodromeSquare),
          (W, IstanbulTaxi)
        ],
      objects =
        [ TurkishDelight,
          Rug
        ]
    }

istanbulHippodromeSquare :: Room
istanbulHippodromeSquare =
  Room
    { rname = IstanbulHippodromeSquare,
      desc = "You are in Hippodrome Square.",
      exits =
        [ (N, IstanbulGrandBazaar),
          (S, IstanbulObeliskOfTheodesius),
          (E, IstanbulTurkishAndIslamicArtMuseum),
          (W, IstanbulSerpentineColumn)
        ],
      objects =
        [
        ]
    }

istanbulObeliskOfTheodesius :: Room
istanbulObeliskOfTheodesius =
  Room
    { rname = IstanbulObeliskOfTheodesius,
      desc = "You are in front of the Obelisk of Theodesius.",
      exits =
        [ (N, IstanbulHippodromeSquare),
          (E, IstanbulBlueMosque)
        ],
      objects =
        [
        ]
    }

istanbulIstiklalAvenue :: Room
istanbulIstiklalAvenue =
  Room
    { rname = IstanbulIstiklalAvenue,
      desc = "You are on İstiklal Avenue.",
      exits =
        [ (E, IstanbulTaksimSquare)
        ],
      objects =
        [ GucciBag
        ]
    }

istanbulTurkishAndIslamicArtMuseum :: Room
istanbulTurkishAndIslamicArtMuseum =
  Room
    { rname = IstanbulTurkishAndIslamicArtMuseum,
      desc = "You are in front of the Turkish and Islamic Art Museum.",
      exits =
        [ (S, IstanbulBlueMosque),
          (E, IstanbulTopkapiPalace),
          (W, IstanbulHippodromeSquare)
        ],
      objects =
        [
        ]
    }

istanbulBlueMosque :: Room
istanbulBlueMosque =
  Room
    { rname = IstanbulBlueMosque,
      desc = "You are in front of the Blue Mosque.",
      exits =
        [ (N, IstanbulTurkishAndIslamicArtMuseum),
          (E, IstanbulHagiaSophia),
          (W, IstanbulObeliskOfTheodesius)
        ],
      objects =
        [
        ]
    }

istanbulBogaziciUniversity :: Room
istanbulBogaziciUniversity =
  Room
    { rname = IstanbulBogaziciUniversity,
      desc = "You are at Boğaziçi University.",
      exits =
        [ (S, IstanbulTaksimSquare)
        ],
      objects =
        [ ChicagoPlaneTicket
        ]
    }

istanbulTaksimSquare :: Room
istanbulTaksimSquare =
  Room
    { rname = IstanbulTaksimSquare,
      desc = "You are in Taksim Square.",
      exits =
        [ (N, IstanbulBogaziciUniversity),
          (S, IstanbulGalataTower),
          (E, IstanbulOrtakoySquare),
          (W, IstanbulIstiklalAvenue)
        ],
      objects =
        [
        ]
    }

istanbulGalataTower :: Room
istanbulGalataTower =
  Room
    { rname = IstanbulGalataTower,
      desc = "You are in front of Galata Tower.",
      exits =
        [ (N, IstanbulTaksimSquare),
          (S, IstanbulTopkapiPalace),
          (E, IstanbulDolmabahcePalace)
        ],
      objects =
        [
        ]
    }

istanbulTopkapiPalace :: Room
istanbulTopkapiPalace =
  Room
    { rname = IstanbulTopkapiPalace,
      desc = "You are in front of Topkapı Palace.",
      exits =
        [ (N, IstanbulGalataTower),
          (S, IstanbulHagiaSophia),
          (W, IstanbulTurkishAndIslamicArtMuseum)
        ],
      objects =
        [
        ]
    }

istanbulHagiaSophia :: Room
istanbulHagiaSophia =
  Room
    { rname = IstanbulHagiaSophia,
      desc = "You are in front of the Hagia Sophia.",
      exits =
        [ (N, IstanbulTopkapiPalace),
          (W, IstanbulBlueMosque)
        ],
      objects =
        [
        ]
    }

istanbulOrtakoySquare :: Room
istanbulOrtakoySquare =
  Room
    { rname = IstanbulOrtakoySquare,
      desc = "You are in Ortaköy Square.",
      exits =
        [ (S, IstanbulDolmabahcePalace),
          (W, IstanbulTaksimSquare)
        ],
      objects =
        [
        ]
    }

istanbulDolmabahcePalace :: Room
istanbulDolmabahcePalace =
  Room
    { rname = IstanbulDolmabahcePalace,
      desc = "You are in front of the Dolmabahçe Palace.",
      exits =
        [ (N, IstanbulOrtakoySquare),
          (W, IstanbulGalataTower)
        ],
      objects =
        [
        ]
    }

----------------------------------
-- End list of predefined Rooms --
----------------------------------

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
    amsterdamVanGoghMuseum,

    -- CDG
    cdgPlane,
    cdgGate,
    cdgCustoms,
    cdgSecurity,
    cdgLobby,
    cdgAirportStation,
    cdgTrain,

    -- Paris
    parGareDuNord,
    parSacreCoeur,
    parMoulinRouge,
    parPalaisGarnier,
    parPalaisRoyal,
    parLouvre,
    parPlaceDeLaConcorde,
    parArcDeTriomphe,
    parPlaceDesVosges,
    parNotreDame,
    parSainteChapelle,
    parHotelDesInvalides,
    parEiffelTower,
    parPantheon,
    parMontparnasseTower,
    parCatacombs,

    -- PRG
    prgPlane,
    prgGate,
    prgCustoms,
    prgSecurity,
    prgLobby,
    prgAirportStation,
    prgTrain,

    -- Prague
    praMainStation,
    praSpanishSynagogue,
    praOldJewishCemetary,
    praPowderGate,
    praTynChurch,
    praHouseOfArts,
    praNationalMuseum,
    praAstronomicalClock,
    praCharlesBridge,
    praChurchOfStNicholas,
    praPragueCastle,
    praNationalTheatre,
    praLennonWall,
    praPetrinTower,
    praStrahovMonastery,
    praDancingHouse,

    -- IST
    istPlane,
    istGate,
    istCustoms,
    istSecurity,
    istLobby,
    istLoadingZone,
    istTaxi,

    -- Istanbul
    istanbulTaxi,
    istanbulSerpentineColumn,
    istanbulSpiceBazaar,
    istanbulGrandBazaar,
    istanbulHippodromeSquare,
    istanbulObeliskOfTheodesius,
    istanbulIstiklalAvenue,
    istanbulTurkishAndIslamicArtMuseum,
    istanbulBlueMosque,
    istanbulBogaziciUniversity,
    istanbulTaksimSquare,
    istanbulGalataTower,
    istanbulTopkapiPalace,
    istanbulHagiaSophia,
    istanbulOrtakoySquare,
    istanbulDolmabahcePalace
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