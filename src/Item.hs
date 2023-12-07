module Item where

import Data.Map qualified as M

data ItemName
  = Pot
  | Homework
  | Rake
  | Knife
  | Fork
  | Bed
  | LondonPlaneTicket
  | LondonTubeTicket
  | LondonReturnTubeTicket
  | AmsterdamPlaneTicket
  | AmsterdamTrainTicket
  | AmsterdamReturnTrainTicket
  | ParisPlaneTicket
  | ParisTrainTicket
  | ParisReturnTrainTicket
  | PraguePlaneTicket
  | PragueTrainTicket
  | PragueReturnTrainTicket
  | IstanbulPlaneTicket
  | ChicagoPlaneTicket
  | TurkishDelight
  | Tulip
  | Rose
  | Daffodil
  | Azalea
  | TeaSet
  | Stroopwafel
  | Croissant
  | RedGarnet
  | BigBook
  | SmallBook
  | CafeteriaFood
  | Cookie
  | Dumbbell
  | Bike
  | TShirt
  | Jeans
  | Suit
  | Lamp
  | ToiletPaper
  | Burger
  | Coffee
  | ShrekDVD
  | WaterBottle
  | Wax
  | Trash
  | Camera
  | BigPainting
  | Bush
  | Leaf
  | Statue
  | SmallPainting
  | Beer
  | MonaLisa
  | Spiderweb
  | Flyer
  | Stick
  | PicnicBlanket
  | PicnicBasket
  | Apple
  | Orange
  | Banana
  | RoastBeefSandwich
  | Spices
  | BlackTea
  | Rug
  | GucciBag
  deriving (Eq, Ord)

instance Show ItemName where
  show Pot = "pot"
  show Homework = "homework"
  show Rake = "rake"
  show Knife = "knife"
  show Fork = "fork"
  show Bed = "bed"
  show LondonPlaneTicket = "london plane ticket"
  show LondonTubeTicket = "westminster tube ticket"
  show LondonReturnTubeTicket = "heathrow airport tube ticket"
  show AmsterdamPlaneTicket = "amsterdam plane ticket"
  show AmsterdamTrainTicket = "centraal train ticket"
  show AmsterdamReturnTrainTicket = "schiphol airport train ticket"
  show ParisPlaneTicket = "paris plane ticket"
  show ParisTrainTicket = "gare du nord train ticket"
  show ParisReturnTrainTicket = "charles de gaulle airport train ticket"
  show PraguePlaneTicket = "prague plane ticket"
  show PragueTrainTicket = "main station train ticket"
  show PragueReturnTrainTicket = "vaclav havel airport train ticket"
  show IstanbulPlaneTicket = "istanbul plane ticket"
  show ChicagoPlaneTicket = "chicago plane ticket"
  show TurkishDelight = "turkish delight"
  show Tulip = "tulip"
  show Rose = "rose"
  show Daffodil = "daffodil"
  show Azalea = "azalea"
  show TeaSet = "tea set"
  show Stroopwafel = "stroopwafel"
  show Croissant = "croissant"
  show RedGarnet = "red garnet"
  show BigBook = "big book"
  show SmallBook = "small book"
  show CafeteriaFood = "cafeteria food"
  show Cookie = "cookies"
  show Dumbbell = "dumbbell"
  show Bike = "bike"
  show TShirt = "t-shirt"
  show Jeans = "jeans"
  show Suit = "suit"
  show Lamp = "lamp"
  show ToiletPaper = "toilet paper"
  show Burger = "burger"
  show Coffee = "coffee"
  show ShrekDVD = "shrek dvd"
  show WaterBottle = "water bottle"
  show Wax = "wax"
  show Trash = "trash"
  show Camera = "camera"
  show BigPainting = "big painting"
  show Bush = "bush"
  show Leaf = "leaf"
  show Statue = "statue"
  show SmallPainting = "small painting"
  show Beer = "beer"
  show MonaLisa = "mona lisa"
  show Spiderweb = "spiderweb"
  show Flyer = "flyer"
  show Stick = "stick"
  show PicnicBlanket = "picnic blanket"
  show PicnicBasket = "picnic basket"
  show Apple = "apple"
  show Orange = "orange"
  show Banana = "banana"
  show RoastBeefSandwich = "roast beef sandwich"
  show Spices = "spices"
  show BlackTea = "black tea"
  show Rug = "rug"
  show GucciBag = "gucci bag"

type Universe = M.Map ItemName Item

data Item = Item {iname :: ItemName, weight :: Integer} deriving (Show)

mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (mkUniverseHelper items)

mkUniverseHelper :: [Item] -> [(ItemName, Item)]
mkUniverseHelper [] = []
mkUniverseHelper (x : xs) = ((iname x, x) : mkUniverseHelper xs)

-- List of predefined Items
pot :: Item
pot = Item {iname = Pot, weight = 30}

homework :: Item
homework = Item {iname = Homework, weight = 5}

rake :: Item
rake = Item {iname = Rake, weight = 20}

knife :: Item
knife = Item {iname = Knife, weight = 6}

fork :: Item
fork = Item {iname = Fork, weight = 6}

bed :: Item
bed = Item {iname = Bed, weight = 200}

londonPlaneTicket :: Item
londonPlaneTicket = Item {iname = LondonPlaneTicket, weight = 1}

londonTubeTicket :: Item
londonTubeTicket = Item {iname = LondonTubeTicket, weight = 1}

londonReturnTubeTicket :: Item
londonReturnTubeTicket = Item {iname = LondonReturnTubeTicket, weight = 1}

amsterdamPlaneTicket :: Item
amsterdamPlaneTicket = Item {iname = AmsterdamPlaneTicket, weight = 1}

amsterdamTrainTicket :: Item
amsterdamTrainTicket = Item {iname = AmsterdamTrainTicket, weight = 1}

amsterdamReturnTrainTicket :: Item
amsterdamReturnTrainTicket =
  Item {iname = AmsterdamReturnTrainTicket, weight = 1}

parisPlaneTicket :: Item
parisPlaneTicket = Item {iname = ParisPlaneTicket, weight = 1}

parisTrainTicket :: Item
parisTrainTicket = Item {iname = ParisTrainTicket, weight = 1}

parisReturnTrainTicket :: Item
parisReturnTrainTicket = Item {iname = ParisReturnTrainTicket, weight = 1}

praguePlaneTicket :: Item
praguePlaneTicket = Item {iname = PraguePlaneTicket, weight = 1}

pragueTrainTicket :: Item
pragueTrainTicket = Item {iname = PragueTrainTicket, weight = 1}

pragueReturnTrainTicket :: Item
pragueReturnTrainTicket = Item {iname = PragueReturnTrainTicket, weight = 1}

istanbulPlaneTicket :: Item
istanbulPlaneTicket = Item {iname = IstanbulPlaneTicket, weight = 1}

chicagoPlaneTicket :: Item
chicagoPlaneTicket = Item {iname = ChicagoPlaneTicket, weight = 1}

turkishDelight :: Item
turkishDelight = Item {iname = TurkishDelight, weight = 10}

tulip :: Item
tulip = Item {iname = Tulip, weight = 2}

rose :: Item
rose = Item {iname = Rose, weight = 2}

daffodil :: Item
daffodil = Item {iname = Daffodil, weight = 2}

azalea :: Item
azalea = Item {iname = Azalea, weight = 2}

teaSet :: Item
teaSet = Item {iname = TeaSet, weight = 15}

stroopwafel :: Item
stroopwafel = Item {iname = Stroopwafel, weight = 6}

croissant :: Item
croissant = Item {iname = Croissant, weight = 6}

redGarnet :: Item
redGarnet = Item {iname = RedGarnet, weight = 3}

bigBook :: Item
bigBook = Item {iname = BigBook, weight = 20}

smallBook :: Item
smallBook = Item {iname = SmallBook, weight = 10}

cafeteriaFood :: Item
cafeteriaFood = Item {iname = CafeteriaFood, weight = 15}

cookie :: Item
cookie = Item {iname = Cookie, weight = 7}

dumbbell :: Item
dumbbell = Item {iname = Dumbbell, weight = 50}

bike :: Item
bike = Item {iname = Bike, weight = 85}

tShirt :: Item
tShirt = Item {iname = TShirt, weight = 7}

jeans :: Item
jeans = Item {iname = Jeans, weight = 9}

suit :: Item
suit = Item {iname = Suit, weight = 13}

lamp :: Item
lamp = Item {iname = Lamp, weight = 30}

toiletPaper :: Item
toiletPaper = Item {iname = ToiletPaper, weight = 11}

burger :: Item
burger = Item {iname = Burger, weight = 10}

coffee :: Item
coffee = Item {iname = Coffee, weight = 6}

shrekDVD :: Item
shrekDVD = Item {iname = ShrekDVD, weight = 14}

waterBottle :: Item
waterBottle = Item {iname = WaterBottle, weight = 8}

wax :: Item
wax = Item {iname = Wax, weight = 2}

trash :: Item
trash = Item {iname = Trash, weight = 4}

camera :: Item
camera = Item {iname = Camera, weight = 23}

bigPainting :: Item
bigPainting = Item {iname = BigPainting, weight = 35}

bush :: Item
bush = Item {iname = Bush, weight = 18}

leaf :: Item
leaf = Item {iname = Leaf, weight = 1}

smallPainting :: Item
smallPainting = Item {iname = SmallPainting, weight = 20}

statue :: Item
statue = Item {iname = Statue, weight = 150}

beer :: Item
beer = Item {iname = Beer, weight = 12}

monaLisa :: Item
monaLisa = Item {iname = MonaLisa, weight = 45}

spiderweb :: Item
spiderweb = Item {iname = Spiderweb, weight = 2}

flyer :: Item
flyer = Item {iname = Flyer, weight = 5}

stick :: Item
stick = Item {iname = Stick, weight = 10}

picnicBlanket :: Item
picnicBlanket = Item {iname = PicnicBlanket, weight = 17}

picnicBasket :: Item
picnicBasket = Item {iname = PicnicBasket, weight = 25}

apple :: Item
apple = Item {iname = Apple, weight = 6}

orange :: Item
orange = Item {iname = Orange, weight = 6}

banana :: Item
banana = Item {iname = Banana, weight = 6}

roastBeefSandwich :: Item
roastBeefSandwich = Item {iname = RoastBeefSandwich, weight = 9}

spices :: Item
spices = Item {iname = Spices, weight = 4}

blackTea :: Item
blackTea = Item {iname = BlackTea, weight = 4}

rug :: Item
rug = Item {iname = Rug, weight = 31}

gucciBag :: Item
gucciBag = Item {iname = GucciBag, weight = 24}

-- Universe
univ :: Universe
univ =
  mkUniverse
    [ pot,
      homework,
      rake,
      knife,
      fork,
      bed,
      londonPlaneTicket,
      londonTubeTicket,
      londonReturnTubeTicket,
      amsterdamPlaneTicket,
      amsterdamTrainTicket,
      amsterdamReturnTrainTicket,
      parisPlaneTicket,
      parisTrainTicket,
      parisReturnTrainTicket,
      praguePlaneTicket,
      pragueTrainTicket,
      pragueReturnTrainTicket,
      istanbulPlaneTicket,
      chicagoPlaneTicket,
      turkishDelight,
      tulip,
      rose,
      daffodil,
      azalea,
      teaSet,
      stroopwafel,
      croissant,
      redGarnet,
      smallBook,
      bigBook,
      cafeteriaFood,
      cookie,
      dumbbell,
      bike,
      tShirt,
      jeans,
      suit,
      lamp,
      toiletPaper,
      burger,
      coffee,
      shrekDVD,
      waterBottle,
      wax,
      trash,
      camera,
      bigPainting,
      bush,
      leaf,
      smallPainting,
      statue,
      beer,
      monaLisa,
      spiderweb,
      flyer,
      stick,
      picnicBlanket,
      picnicBasket,
      apple,
      orange,
      banana,
      roastBeefSandwich,
      spices,
      blackTea,
      rug,
      gucciBag
    ]

-- Item Names
itemNames :: [ItemName]
itemNames = map iname $ M.elems univ