import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (tails, subsequences)
import Debug.Trace (trace)

data Rectangle = Rectangle { id :: Int
                           , top :: Int
                           , right :: Int
                           , bottom :: Int
                           , left :: Int
                           } deriving (Read, Show)

main = do     
    contents <- getContents
    let originalRectangles = mapMaybe claimIntoRectangle $ lines contents
    print (getAllRectanglePermutations $ originalRectangles)

claimIntoRectangle :: String -> Maybe Rectangle
claimIntoRectangle claim =
  let id = readMaybe $ takeBetweenChars '#' '@' claim :: Maybe Int
      left = readMaybe $ takeBetweenChars '@' ',' claim :: Maybe Int
      top = readMaybe $ takeBetweenChars ',' ':' claim :: Maybe Int
      width = readMaybe $ takeBetweenChars ':' 'x' claim :: Maybe Int
      height = readMaybe $ takeBetweenChars 'x' ' ' claim :: Maybe Int
  in  rectangleFromString id (left, top, width, height)


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

-- createRectangles :: [(Int, Int, Int, Int)] -> [Rectangle]
-- createRectangles = filterMap rectangleFromString

rectangleFromString :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> Maybe Rectangle
rectangleFromString id (Just left, Just top, Just width, Just height) =
  newRectangle id top (left + width) (top + height) left
rectangleFromString _ = Nothing

newRectangle :: Int -> Int -> Int -> Int -> Maybe Rectangle
newRectangle top right bottom left
  | bottom < top        = Nothing
  | right < left        = Nothing
  | (right - left) == 0 = Nothing
  | (bottom - top) == 0 = Nothing
  | otherwise           = Just (Rectangle top right bottom left)

area :: Rectangle -> Int
area (Rectangle top right bottom left) =
  (right - left) * (bottom - top)

intersection :: Rectangle -> Rectangle -> Maybe Rectangle
intersection (Rectangle t1 r1 b1 l1) (Rectangle t2 r2 b2 l2) =
  newRectangle (min t1 t2) (min r1 r2) (max b1 b2) (max l1 l2)

getIntersectionOf :: [Rectangle] -> Maybe Rectangle
getIntersectionOf [] = Nothing
getIntersectionOf (x:[]) = Nothing
getIntersectionOf (x:y:[]) = intersection x y
getIntersectionOf (x:xs) =
  case getIntersectionOf xs of Nothing -> Nothing
                               Just i -> intersection x $ i

perms :: Int -> [a] -> [[a]]
perms _ [] = []
perms 0 list = [[]]
perms 1 list = map (:[]) list
perms n (x:xs) = (map (x:) $ perms (n-1) xs) ++ (perms n xs)

getAllRectanglePermutations :: [Rectangle] -> Int
getAllRectanglePermutations rectangles =
  -- getPermsUpTo rectangles 2 0
  getAllPerms rectangles

getPermsUpTo :: [Rectangle] -> Int -> Int -> Int
getPermsUpTo r n sum =
  let permutations = trace (show n) (perms n r)
      area = trace (show sum) (reducePerms 0 permutations)
  in if n > (length r) then sum
                       else if area == 0 then sum
                                        else getPermsUpTo r (n+1) (sum+area)

getAllPerms rectangles =
  reducePerms 0 (subsequences rectangles)


data Intersection = (Rectangle, [Int])

reducePerms :: Int -> [[Rectangle]] -> Int
reducePerms n [] = n
reducePerms n (x:xs) =
  let intersection = getIntersectionOf x
      intersectionArea = maybe 0 area intersection
      m = multiplier $ length x
  in reducePerms ((intersectionArea * m) + n) xs


multiplier :: Int -> Int
multiplier n = if n `mod` 2 == 0 then 1 else -1

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) =
    let next = filterMap f xs
    in maybe next (:next) $ f x


