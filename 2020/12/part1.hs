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

data DirectionX = East | West deriving (Show, Eq)
data DirectionY = North | South deriving (Show, Eq)
data Direction = EW DirectionX | NS DirectionY deriving (Show, Eq)
data Side = L | R deriving (Show, Eq)
data Action = Forward | Move Direction | Turn Side deriving (Show)
data Instruction = Instruction Action Int deriving (Show)

data Position a = Position a Int deriving (Show)
data Ship = Ship
    { facing :: Int
    , x :: Position DirectionX
    , y :: Position DirectionY
    } deriving (Show)


main :: IO ()
main = do
    input <- lines <$> getContents
    let instructions = mapMaybe parseInstruction input :: [Instruction]
    let ship = Ship 0 (Position East 0) (Position North 0)
    print $ manhattanDistance $ foldl navigate ship instructions


manhattanDistance :: Ship -> Int
manhattanDistance ship =
    case (x ship, y ship) of
        (Position _ xP, Position _ yP) -> xP + yP


navigate :: Ship -> Instruction -> Ship
navigate ship (Instruction action value) =
    case action of
        Forward -> move ship (fromAngle $ facing ship) value
        Move direction -> move ship direction value
        Turn side -> ship { facing = turn (facing ship) side value }


fromAngle :: Int -> Direction
fromAngle angle | angle < 90 = EW East
fromAngle angle | angle < 180 = NS South
fromAngle angle | angle < 270 = EW West
fromAngle _ = NS North


turn :: Int -> Side -> Int -> Int
turn angle L by = (angle - by) `mod` 360
turn angle R by = (angle + by) `mod` 360


move :: Ship -> Direction -> Int -> Ship
move ship (EW direction) value = ship { x = align (x ship) direction value }
move ship (NS direction) value = ship { y = align (y ship) direction value }


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
parseAction 'F' = Just $ Forward
parseAction _ = Nothing