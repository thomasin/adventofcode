import Debug.Trace (trace)
import Data.List (minimumBy)
import Data.Tuple (snd)

main = do
  contents <- readFile "05.txt"
  print $ minimumBy compareLengths $ getRemainingLengths contents [] ['a'..'z']

compareLengths (_, x) (_, y) = compare x y

getRemainingLengths :: String -> [(Char, Int)] -> String -> [(Char, Int)]
getRemainingLengths _ results [] = results
getRemainingLengths contents results (x:xs) =
  let result = (x, length $ deletePolarisingPairsWith arePolarising [] $ filter ( isNotLetter x ) contents )
  in getRemainingLengths contents (result:results) xs

deletePolarisingPairsWith :: (Char -> Char -> Bool) -> String -> String -> String
deletePolarisingPairsWith _ remaining [] = remaining
deletePolarisingPairsWith _ remaining (x:[]) = remaining
deletePolarisingPairsWith f remaining (x:y:[]) = if f x y then remaining else (x:y:remaining)
deletePolarisingPairsWith f [] (x:y:z:xs) =
  if f x y then deletePolarisingPairsWith f [] (z:xs)
           else if f y z then deletePolarisingPairsWith f [] (x:xs)
                         else deletePolarisingPairsWith f [x] (y:z:xs)

deletePolarisingPairsWith f (r:rs) (x:y:z:xs) =
  if f x y then deletePolarisingPairsWith f rs (r:z:xs)
           else if f y z then deletePolarisingPairsWith f (r:rs) (x:xs)
                         else deletePolarisingPairsWith f (x:r:rs) (y:z:xs)

arePolarising :: Char -> Char -> Bool
arePolarising char1 char2 =
  let e1 = fromEnum char1
      e2 = fromEnum char2
  in ((==) 32 $ max e1 e2 - min e1 e2)


isNotLetter :: Char -> Char -> Bool
isNotLetter matchChar char =
  char /= matchChar && ( (/=) matchChar $ toEnum $ (+) 32 $ fromEnum char )
