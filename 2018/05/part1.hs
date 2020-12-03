import Debug.Trace (trace)

main = do
  contents <- readFile "05.txt"
  print $ length $ deletePolarisingPairs [] contents


deletePolarisingPairs :: String -> String -> String
deletePolarisingPairs remaining [] = remaining
deletePolarisingPairs remaining (x:[]) = remaining
deletePolarisingPairs remaining (x:y:[]) = if arePolarising x y then remaining else (x:y:remaining)
deletePolarisingPairs [] (x:y:z:xs) =
  if arePolarising x y then deletePolarisingPairs [] (z:xs)
                       else if arePolarising y z then deletePolarisingPairs [] (x:xs)
                                                 else deletePolarisingPairs [x] (y:z:xs)

deletePolarisingPairs (r:rs) (x:y:z:xs) =
  if arePolarising x y then deletePolarisingPairs rs (r:z:xs)
                       else if arePolarising y z then deletePolarisingPairs (r:rs) (x:xs)
                                                 else deletePolarisingPairs (x:r:rs) (y:z:xs)


arePolarising :: Char -> Char -> Bool
arePolarising char1 char2 =
  (==) 32 $ max (fromEnum char1) (fromEnum char2) - min (fromEnum char1) (fromEnum char2)

