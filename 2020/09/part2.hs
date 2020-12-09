{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base text vector"
 --ghc-options=-Wall
-}


import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Arrow ((&&&))


invalidNumber :: Int
invalidNumber = 507622668


main :: IO ()
main = do
    input <- mapMaybe readMaybe <$> lines <$> getContents
    print $ uncurry (+) <$> (minimum &&& maximum) <$> findContiguousSet input [] 0


findContiguousSet :: [Int] -> [Int] -> Int -> Maybe [Int]
findContiguousSet [] _ _ = Nothing
findContiguousSet (x:xs) [] total = findContiguousSet xs [x] (total + x)
findContiguousSet nums@(x:xs) set@(y:ys) total
    | x + total == invalidNumber = Just (set ++ [x])
    | x + total > invalidNumber = findContiguousSet nums ys (total - y)
    | otherwise = findContiguousSet xs (set ++ [x]) (total + x)
