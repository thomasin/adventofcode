import Data.List (nub, sort, stripPrefix)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

main = do
  contents <- readFile "07.txt"
  let directions = mapMaybe turnIntoNumbers $ lines contents
      possibilities = nub $ foldl (\xs (a, b) -> a:b:xs) [] directions
  print $ reverse $ runWorkers directions 0 ((replicate 5 $ worker Nothing 0), "", "", possibilities)
  print possibilities


turnIntoNumbers :: String -> Maybe (Char, Char)
turnIntoNumbers str =
  stripPrefix "Step " str >>= getSecondNumber

getSecondNumber :: String -> Maybe (Char, Char)
getSecondNumber (x:xs) =
  fmap (\(y:ys) -> (x, y))
  $ stripPrefix " must be finished before step " xs


type WorkerResult = (Maybe Char, Int, String, String)

runWorkers :: [(Char, Char)] -> Int -> ([String -> String -> WorkerResult], String, String, String) -> String
runWorkers _ _ (_, done, "", "") = done
runWorkers remaining time (workers, done, next, possibilities) =
  runWorkers remaining time
  $ foldl (tickWorker remaining) ([], done, next, possibilities) workers


tickWorker :: [(Char, Char)] -> ([(String -> String -> WorkerResult)], String, String, String) -> (String -> String -> WorkerResult) -> ([(String -> String -> WorkerResult)], String, String, String)
tickWorker remaining (oldWorkers, pDone, pNext, pPoss) w =
  let nNext = sort $ nub $ (++) pNext $ available pDone pPoss remaining
      nPoss = filter (\p -> p `notElem` nNext) pPoss
  in case w pDone nNext of
    (workingOn, newTime, newDone, newNext) ->
      ((++) oldWorkers [worker workingOn newTime], newDone, newNext, nPoss)

worker :: Maybe Char -> Int -> String -> String -> WorkerResult
worker w t d n | trace (show w ++ " " ++ show t ++ " " ++ show d ++ " " ++ show n) False = undefined
worker Nothing time done "" = (Nothing, 0, done, "")
worker Nothing time done (x:xs) = (Just x, 0, done, xs)
worker (Just c) time done next 
  | takesTime c == time + 1 = (Nothing, 0, c:done, next)
  | otherwise = (Just c, time + 1, done, next)


takesTime char = (fromEnum char) - 4


available :: String -> String -> [(Char, Char)] -> String
available done possibilities remaining =
  possibilitiesNotPresent possibilities
  $ map snd
  $ filter (\(f, t) -> f `notElem` done) remaining

possibilitiesNotPresent :: String -> String -> String
possibilitiesNotPresent possibilities xs =
  filter (\p -> p `notElem` xs) possibilities
