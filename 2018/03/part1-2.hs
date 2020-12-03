import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (maximum)
import Debug.Trace (trace)

data Rectangle = Rectangle { top :: Int
                           , right :: Int
                           , bottom :: Int
                           , left :: Int
                           } deriving (Read, Show)

main = do     
    contents <- getContents
    let originalRectangles = mapMaybe claimIntoRectangle $ lines contents
        matrix = stickRectangles originalRectangles $ buildMatrix originalRectangles
    print . sumMatrix 0 $ concat matrix

--

sumMatrix :: Int -> [Maybe Int] -> Int
sumMatrix sum [] = sum
sumMatrix sum (m:ms) =
  case m of Nothing -> sumMatrix sum ms
            Just int -> sumMatrix (sum + int) ms

buildMatrix :: [Rectangle] -> [[Maybe Int]]
buildMatrix rectangles =
  let width = maximum $ map right rectangles
      height = maximum $ map bottom rectangles
  in replicate height $ replicate width Nothing


stickRectangles :: [Rectangle] -> [[Maybe Int]] -> [[Maybe Int]]
stickRectangles [] matrix = matrix
stickRectangles ((Rectangle top right bottom left):xs) matrix =
  stickRectangles xs $ applyWith top bottom 0 (applyWith left right 0 getCell) matrix

getCell c = if c == Nothing then Just 0 else Just 1

applyWith :: Int -> Int -> Int -> (a -> a) -> [a] -> [a]
applyWith _ _ _ _ [] = []
applyWith dimMin dimMax i f (x:xs) =
  let next = applyWith dimMin dimMax (i + 1) f xs
  in if i < dimMin || i >= dimMax then (x:next) else ((f x):next)


-- 


rectangleFromString :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> Maybe Rectangle
rectangleFromString (Just left, Just top, Just width, Just height) =
  newRectangle top (left + width) (top + height) left
rectangleFromString _ = Nothing

newRectangle :: Int -> Int -> Int -> Int -> Maybe Rectangle
newRectangle top right bottom left
  | bottom < top        = Nothing
  | right < left        = Nothing
  | (right - left) == 0 = Nothing
  | (bottom - top) == 0 = Nothing
  | otherwise           = Just (Rectangle top right bottom left)  -- SWITCHED AROUND

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) =
    let next = filterMap f xs
    in maybe next (:next) $ f x

claimIntoRectangle :: String -> Maybe Rectangle
claimIntoRectangle claim =
  let id = readMaybe $ takeBetweenChars '#' '@' claim :: Maybe Int
      left = readMaybe $ takeBetweenChars '@' ',' claim :: Maybe Int
      top = readMaybe $ takeBetweenChars ',' ':' claim :: Maybe Int
      width = readMaybe $ takeBetweenChars ':' 'x' claim :: Maybe Int
      height = readMaybe $ takeBetweenChars 'x' ' ' claim :: Maybe Int
  in  rectangleFromString (left, top, width, height)

-- removes all spaces
splitAround :: Char -> String -> (String, String)
splitAround _ [] = ([], [])
splitAround char (x:xs) =
  if char == x then ([], xs)
               else let (pre, rest) = splitAround char xs
                    in if x == ' ' then (pre, rest)
                                   else ((x:pre), rest)

takeBetweenChars :: Char -> Char -> String -> String
takeBetweenChars char1 char2 string =
  let (_, f) = splitAround char1 string
      (s, _) = splitAround char2 f
  in s
