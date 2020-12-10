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
import Data.Map (Map, singleton, insertWith, findWithDefault)


main :: IO ()
main = do
    input <- mapMaybe readMaybe <$> lines <$> getContents
    let (differences, _) = foldl countDifferences (singleton 3 1, 0) $ sort input
    print $ findWithDefault 0 1 differences * findWithDefault 0 3 differences


countDifferences :: (Map Int Int, Int) -> Int -> (Map Int Int, Int)
countDifferences (differences, joltage) rating =
    (insertWith (+) (rating-joltage) 1 differences, rating)
