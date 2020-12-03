{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "containers"
 --ghc-options=-Wall
-}

import qualified Data.IntSet as IntSet


-- main :: IO ()
-- main = do
--     input <- getContents
--     let numbers = IntSet.fromList $ stringToNumbers $ input
--     print (IntSet.member 1934 numbers)

-- stringToNumbers :: String -> [Int] 
-- stringToNumbers =
--     map read . lines


main :: IO ()
main = do
    input <- getContents
    let numbersList = map read $ lines $ input
    let numbersSet = IntSet.fromList numbersList
    print (find2020s numbersSet numbersList)


find2020s :: IntSet.IntSet -> [Int] -> Maybe Int
find2020s _ [] = Nothing
find2020s numbers (x:xs)
    | IntSet.member (2020 - x) numbers = Just (x * (2020 - x))
    | otherwise = find2020s numbers xs