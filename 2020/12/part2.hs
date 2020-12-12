{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base"
 --ghc-options=-Wall
-}


import Data.Maybe
import Text.Read
import Control.Monad


data DirectionX = East | West deriving (Show, Eq)
data DirectionY = North | South deriving (Show, Eq)
data Direction = EW DirectionX | NS DirectionY deriving (Show, Eq)
data Side = L | R deriving (Show, Eq)
data Action = Forwards | Move Direction | Turn Side deriving (Show)
data Instruction = Instruction Action Int deriving (Show)

data Position a = Position a Int deriving (Show)
data Ship = Ship XY deriving (Show)
data Waypoint = Waypoint XY deriving (Show)
data XY = XY
    { x :: Position DirectionX
    , y :: Position DirectionY
    } deriving (Show)


values :: XY -> ((Direction, Int), (Direction, Int))
values xy =
    let (Position dX vX) = x xy
        (Position dY vY) = y xy
    in ((EW dX, vX), (NS dY, vY))


main :: IO ()
main = do
    input <- lines <$> getContents
    let instructions = mapMaybe parseInstruction input :: [Instruction]
    let ship = Ship $ XY (Position East 0) (Position North 0)
    let waypoint = Waypoint $ XY (Position East 10) (Position North 1)
    let result = foldM navigate (ship, waypoint) instructions
    print $ manhattanDistance <$> fst <$> result


manhattanDistance :: Ship -> Int
manhattanDistance (Ship ship) =
    let ((_, vX), (_, vY)) = values ship
    in vX + vY


navigate :: (Ship, Waypoint) -> Instruction -> Maybe (Ship, Waypoint)
navigate (ship, waypoint@(Waypoint wp)) (Instruction action value) =
    case action of
        Forwards ->
            Just ( forwards ship waypoint value, waypoint )

        Move direction ->
            Just ( ship, Waypoint $ move direction value wp )

        Turn side ->
            (,) ship . Waypoint <$> rotate wp side value

-- angles

fromAngle :: Int -> Direction
fromAngle angle | angle < 90 = EW East
fromAngle angle | angle < 180 = NS South
fromAngle angle | angle < 270 = EW West
fromAngle _ = NS North


toAngle :: Direction -> Int
toAngle (EW East) = 0
toAngle (NS South) = 90
toAngle (EW West) = 180
toAngle (NS North) = 270

-- rotation

turn :: Side -> Int -> Direction -> Direction
turn side by direction =
    let turnBy a = if side == L then a - by else a + by
    in fromAngle $ flip mod 360 $ turnBy $ toAngle direction


rotate :: XY -> Side -> Int -> Maybe XY
rotate xy side value =
    let ((dX, vX), (dY, vY)) = values xy
    in case apply (turn side value) dX dY of
        (EW ew, NS ns) -> Just $ XY (Position ew vX) (Position ns vY)
        (NS ns, EW ew) -> Just $ XY (Position ew vY) (Position ns vX)
        _ -> Nothing


apply :: (a -> b) -> a -> a -> (b, b)
apply f a b = (f a, f b)


-- forwards

forwards :: Ship -> Waypoint -> Int -> Ship
forwards (Ship ship) (Waypoint waypoint) value =
    let ((dX, vX), (dY, vY)) = values waypoint
    in  Ship $ move dY (vY * value) $ move dX (vX * value) $ ship


-- move

move :: Direction -> Int -> XY -> XY
move (EW direction) value xy = xy { x = align (x xy) direction value }
move (NS direction) value xy = xy { y = align (y xy) direction value }


align :: (Eq a) => Position a -> a -> Int -> Position a
align (Position from value) to by
    | from == to = Position from (value + by)
    | value - by >= 0 = Position from $ value - by
    | otherwise = Position to $ abs $ value - by


-- Parse instructions


parseInstruction :: String -> Maybe Instruction
parseInstruction [] = Nothing
parseInstruction (action:value) = Instruction <$> parseAction action <*> readMaybe value


parseAction :: Char -> Maybe Action
parseAction 'N' = Just $ Move (NS North)
parseAction 'S' = Just $ Move (NS South)
parseAction 'E' = Just $ Move (EW East)
parseAction 'W' = Just $ Move (EW West)
parseAction 'L' = Just $ Turn L
parseAction 'R' = Just $ Turn R
parseAction 'F' = Just $ Forwards
parseAction _ = Nothing