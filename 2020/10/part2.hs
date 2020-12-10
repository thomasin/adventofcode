{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base text containers"
 --ghc-options=-Wall
-}


import Data.Maybe (mapMaybe)
import Text.Read
import Data.List


main :: IO ()
main = do
    input <- sort <$> mapMaybe readMaybe <$> lines <$> getContents
    let branches = findBranches [] (0:input) :: [Int]
    let pathways = head $ foldl (\t x -> (sum $ take x t):t) [1] branches :: Int
    print $ pathways


-- For every value in the list, return how many branches it connects to
-- i.e. [0,1,2,3,6] would return [3,2,1,1,1]. The last number is set to 1.
-- A valid branch is a number that is within 3 of the value.
findBranches :: [Int] -> [Int] -> [Int]
findBranches tree [] = tree
findBranches tree (_:[]) = (1:tree)
findBranches tree (x1:xs) =
    let branches = foldl (\a -> (+) a . inRange x1) 0 (take 3 xs)
    in findBranches (branches:tree) xs

inRange :: Int -> Int -> Int
inRange value x = fromEnum $ x - value < 4
