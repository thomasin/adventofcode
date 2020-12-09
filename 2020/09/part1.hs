{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base text vector"
 --ghc-options=-Wall
-}


import Data.Maybe (mapMaybe)
import Text.Read
import Data.List


main :: IO ()
main = do
    input <- mapMaybe readMaybe <$> lines <$> getContents
    print $ findNonProduct $ splitAt 25 input


findNonProduct :: ([Int], [Int]) -> Maybe Int
findNonProduct (_, []) = Nothing
findNonProduct (preamble@(p:ps), (x:xs)) =
    if productExists preamble x then
        findNonProduct (ps ++ [x], xs)
    else
        Just x


productExists [] _ = False
productExists (p:ps) x
    | (x - p) == p = productExists ps x
    | elem (x - p) ps = True
    | otherwise = productExists ps x
