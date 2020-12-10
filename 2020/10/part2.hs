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
    let branches = countBranches (0:input) :: [Int]
    print $ head $ branches


countBranches :: [Int] -> [Int]
countBranches [] = []
countBranches (x:xs) =
    let branches = length $ takeWhile ((>) 4 . flip (-) x) xs
    in case countBranches xs of
        [] -> [1]
        rest -> (sum $ take branches $ rest):rest
