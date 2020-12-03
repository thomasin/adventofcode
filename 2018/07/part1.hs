import Data.List (stripPrefix, nub, notElem, sort, head, tail)
import Data.Maybe (mapMaybe)

main = do
  contents <- readFile "07.txt"
  let directions = mapMaybe turnIntoNumbers $ lines contents
      possibilities = nub $ foldl (\xs (a, b) -> a:b:xs) [] directions
  print $ reverse $ ready "" "" possibilities directions

turnIntoNumbers :: String -> Maybe (Char, Char)
turnIntoNumbers str =
  stripPrefix "Step " str >>= getSecondNumber

getSecondNumber :: String -> Maybe (Char, Char)
getSecondNumber (x:xs) =
  fmap (\(y:ys) -> (x, y))
  $ stripPrefix " must be finished before step " xs

-- ready :: String -> String -> String -> [(Char, Char)] -> String
ready done "" "" _ = done
ready done next possibilities remaining =
  let newNext = sort $ (++) next $ available done possibilities remaining
  in case newNext of [] -> ready done [] possibilities remaining
                     (x:xs) ->
                        if x `elem` done then ready done xs (filter (\p -> p /= x) possibilities) remaining
                                         else ready (x:done) xs (filter (\p -> p /= x) possibilities) remaining


available :: String -> String -> [(Char, Char)] -> String
available done possibilities remaining =
  possibilitiesNotPresent possibilities
  $ map snd
  $ filter (\(f, t) -> f `notElem` done) remaining

possibilitiesNotPresent :: String -> String -> String
possibilitiesNotPresent possibilities xs =
  filter (\p -> p `notElem` xs) possibilities
