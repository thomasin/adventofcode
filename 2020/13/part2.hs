{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base megaparsec split"
 --ghc-options=-Wall
-}


import Data.Maybe
import Text.Read
import Data.List.Split


{--
working off of http://homepages.math.uic.edu/~leon/mcs425-s08/handouts/chinese_remainder.pdf
--}


main :: IO ()
main = do
    input <- last <$> lines <$> getContents
    let numbers = map readMaybe $ splitOn "," input
    let bigM = product $ catMaybes numbers
    let x = remainderTheorem bigM 0 0 numbers
    print $ maybe "cant calculate ):" (show . flip mod bigM) x


remainderTheorem :: Int -> Int -> Int -> [Maybe Int] -> Maybe Int
remainderTheorem _ _ t [] = Just t
remainderTheorem bigM index t (Nothing:ms) = remainderTheorem bigM (index + 1) t ms 
remainderTheorem bigM index t ((Just m):ms) =
    let a = mod (m - index) m
        z = div bigM m
        inverse = modularInverse (mod z m) m m
    in case inverse of
        Just y -> remainderTheorem bigM (index + 1) (t + (a * z * y)) ms
        Nothing -> Nothing


-- find x given (a * x) `mod` n = 1
modularInverse :: Int -> Int -> Int -> Maybe Int
modularInverse a b n
    | b < 0 = Nothing
    | mod (a * b) n == 1 = Just b
    | otherwise = modularInverse a (b - 1) n








