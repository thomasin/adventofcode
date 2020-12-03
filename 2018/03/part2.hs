import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Data.List (null, notElem, nub)

data Rectangle = Rectangle { id :: Int
                           , top :: Int
                           , right :: Int
                           , bottom :: Int
                           , left :: Int
                           } deriving (Read, Show)

main = do     
    contents <- getContents
    let originalRectangles = mapMaybe claimIntoRectangle $ lines contents
        intersects = intersectingIds originalRectangles []
    print (show intersects)
    print $ filter ( notIntersect intersects ) originalRectangles 

intersectingIds :: [Rectangle] -> [Int] -> [Int]
intersectingIds [] is = nub is
intersectingIds (r:rs) is =
  let intersects = anyIntersection r [] rs
  in intersectingIds rs (intersects ++ is)

notIntersect is r = (Main.id r) `notElem` is

anyIntersection :: Rectangle -> [Int] -> [Rectangle] -> [Int]
anyIntersection rect is [] = if null is then is else (Main.id rect):is
anyIntersection rect is (r:rs) =
  if intersection rect r then anyIntersection rect ((Main.id r):is) rs
                         else anyIntersection rect is rs

intersection :: Rectangle -> Rectangle -> Bool
intersection rect1@(Rectangle _ y a b x) rect2@(Rectangle id y1 a1 b1 x1) =
  if a < x1 || a1 < x || b < y1 || b1 < y then False else True


rectangleFromString :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> Maybe Rectangle
rectangleFromString (Just id, Just left, Just top, Just width, Just height) =
  newRectangle id top (left + width - 1) (top + height - 1) left
rectangleFromString _ = Nothing

newRectangle :: Int -> Int -> Int -> Int -> Int -> Maybe Rectangle
newRectangle id top right bottom left
  | bottom < top        = Nothing
  | right < left        = Nothing
  | (right - left) == 0 = Nothing
  | (bottom - top) == 0 = Nothing
  | otherwise           = Just (Rectangle id top right bottom left)

claimIntoRectangle :: String -> Maybe Rectangle
claimIntoRectangle claim =
  let id = readMaybe $ takeBetweenChars '#' '@' claim :: Maybe Int
      left = readMaybe $ takeBetweenChars '@' ',' claim :: Maybe Int
      top = readMaybe $ takeBetweenChars ',' ':' claim :: Maybe Int
      width = readMaybe $ takeBetweenChars ':' 'x' claim :: Maybe Int
      height = readMaybe $ takeBetweenChars 'x' ' ' claim :: Maybe Int
  in  rectangleFromString (id, left, top, width, height)

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