import Data.List (maximumBy, minimumBy)
import Debug.Trace (trace)

main = do
  contents <- readFile "06.txt"
  let coOrdinates = map toCoordinate $ lines contents
      boundingBox = findBoundingBox coOrdinates
  -- writeFile "output2.txt" $ show 
  print $ getCoOrdinateAreas boundingBox (map (\c -> (c, 0)) coOrdinates) $ concat $ emptyCells coOrdinates boundingBox
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


getCoOrdinateAreas :: ((Int, Int), (Int, Int)) -> [((Int, Int), Int)] -> [Cell] -> [((Int, Int), Int)]
getCoOrdinateAreas _ tally [] = tally
getCoOrdinateAreas bb@((minX, minY), (maxX, maxY)) tally ((Cell (cX, cY) closestCoOrds):cs) =
  case closestCoOrds of (x:[]) -> if cX == minX || cX == maxX || cY == minY || cY == maxY then getCoOrdinateAreas bb (removeFromTally closestCoOrds tally) cs
                                                                                          else getCoOrdinateAreas bb (updateTally x tally) cs
                        [] -> getCoOrdinateAreas bb tally cs
                        (x:xs) -> getCoOrdinateAreas bb tally cs


updateTally :: (Int, Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
updateTally coOrd tally =
  map (\(c, i) -> if c == coOrd then (c, i+1) else (c, i)) tally


removeFromTally :: [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
removeFromTally coOrds tally =
  filter (\(c, _) -> notElem c coOrds) tally


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
  Cell (x, y) $ foldl (getClosestCoOrds (x, y)) [] coOrdinates


getClosestCoOrds :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
getClosestCoOrds _ [] coOrd = [coOrd]
getClosestCoOrds cell old@(c:_) coOrd
  | (distanceBetween cell coOrd) < (distanceBetween cell c) = [coOrd]
  | (distanceBetween cell coOrd) == (distanceBetween cell c) = coOrd:old
  | otherwise = old

compareFirst (x, _) (x1, _) = compare x x1
compareSecond (_, y) (_, y1) = compare y y1

data Cell = Cell (Int, Int) [(Int, Int)] deriving (Show)
