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


invalidNumber :: Int
invalidNumber = 507622668


main :: IO ()
main = do
    input <- mapMaybe readMaybe <$> lines <$> getContents
    print $ uncurry (+) <$> intoTuple minimum maximum <$> findContiguousSet input [] 0


intoTuple :: (x -> a) -> (x -> b) -> x -> (a, b)
intoTuple fa fb x = (fa x, fb x)


findContiguousSet :: [Int] -> [Int] -> Int -> Maybe [Int]
findContiguousSet [] set total = if total == invalidNumber then Just set else Nothing
findContiguousSet (x:xs) [] total = findContiguousSet xs [x] (total + x)
findContiguousSet nums@(x:xs) set@(y:ys) total
    | total == invalidNumber = Just (y:ys)
    | total > invalidNumber = findContiguousSet nums ys (total - y)
    | otherwise = findContiguousSet xs (set ++ [x]) (total + x)
