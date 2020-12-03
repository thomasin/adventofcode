import Data.List (maximumBy, minimumBy)
import Debug.Trace (trace)

main = do
  contents <- readFile "06.txt"
  let coOrdinates = map toCoordinate $ lines contents
      boundingBox = findBoundingBox coOrdinates
  -- writeFile "output2.txt" $ show 
  print $ length $ totalDistanceLessThan 10000 $ concat $ emptyCells coOrdinates boundingBox
  -- getCoOrdinateAreas boundingBox (map (\c -> (c, 0)) coOrdinates) $ concat $
  -- 

toCoordinate :: String -> (Int, Int)
toCoordinate str = read ('(':str ++ ")") :: (Int, Int)

distanceBetween :: (Int, Int) -> (Int, Int) -> Int
distanceBetween (x, y) (x1, y1) = abs ( x1 - x ) + abs ( y1 - y )

findBoundingBox :: [(Int, Int)] -> ((Int, Int), (Int, Int))
findBoundingBox coOrds =
  ( ( (+) (-1) $ fst $ minimumBy compareFirst coOrds, (+) (-1) $ snd $ minimumBy compareSecond coOrds )
  , ( (+) 1 $ fst $ maximumBy compareFirst coOrds, (+) 1 $ snd $ maximumBy compareSecond coOrds )
  )

totalDistanceLessThan distance cells =
  filter (\(Cell _ n) -> n < distance) cells

emptyCells :: [(Int, Int)] -> ((Int, Int), (Int, Int)) -> [[Cell]]
emptyCells coOrdinates ((x1, y1), (x2, y2)) =
  let minX = min x1 x2
      maxX = max x1 x2
      minY = min y1 y2
      maxY = max y1 y2
  in map ( createRow coOrdinates minX maxX ) [minY..maxY]


createRow :: [(Int, Int)] -> Int -> Int -> Int -> [Cell]
createRow coOrdinates minX maxX y =
  map (\x -> createCell coOrdinates (x, y)) [minX..maxX]


createCell :: [(Int, Int)] -> (Int, Int) -> Cell
createCell coOrdinates (x, y) =
  Cell (x, y) $ foldl (sumCoOrds (x, y)) 0 coOrdinates


sumCoOrds :: (Int, Int) -> Int -> (Int, Int) -> Int
sumCoOrds cell sum coOrd =
  (+) sum $ distanceBetween cell coOrd

compareFirst (x, _) (x1, _) = compare x x1
compareSecond (_, y) (_, y1) = compare y y1

data Cell = Cell (Int, Int) Int deriving (Show)
