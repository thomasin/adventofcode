{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "containers"
 --ghc-options=-Wall
-}

import qualified Data.IntSet as IntSet
import Data.List


main :: IO ()
main = do
    input <- getContents
    let numbers = map read $ lines $ input
    let smallNumbers = sort $ filter ((>=) 1010) numbers
    let numberSet = IntSet.fromList numbers
    print (find2020s smallNumbers (tail smallNumbers) numberSet)


find2020s :: [Int] -> [Int] -> IntSet.IntSet -> Maybe Int
find2020s [] _ _ = Nothing
find2020s (_:_:[]) [] _ = Nothing
find2020s (_:[]) [] _ = Nothing
find2020s (_:x1b:x1s) [] numberSet = find2020s (x1b:x1s) x1s numberSet
find2020s primary@(x1:_) (x2:x2s) numberSet
    | hasOpposing x1 x2 numberSet = Just (x1 * x2 * (opposing x1 x2))
    | otherwise = find2020s primary x2s numberSet


hasOpposing :: Int -> Int -> IntSet.IntSet -> Bool
hasOpposing x1 x2 numbers =
    IntSet.member (opposing x1 x2) numbers


opposing :: Int -> Int -> Int
opposing x1 x2 = 2020 - x1 - x2