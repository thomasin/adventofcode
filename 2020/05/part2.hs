{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base"
 --ghc-options=-Wall
-}


import qualified Numeric as Numeric
import qualified Data.Char as Char
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.Maybe as Maybe


main :: IO ()
main = do
    input <- lines <$> getContents
    let boardingPasses = allBoardingPasses input
    let passIds = List.sort $ map passId $ boardingPasses
    print $ [head passIds..last passIds] \\ passIds


--

passId :: (Int, Int) -> Int
passId (row, col) = (row * 8) + col


-- Boarding pass generation

allBoardingPasses :: [String] -> [(Int, Int)]
allBoardingPasses = Maybe.mapMaybe attemptBoardingPass


attemptBoardingPass :: String -> Maybe (Int, Int)
attemptBoardingPass possiblePass =
    let (rowMarker, columnMarker) = splitAt 7 possiblePass
        maybeRow = createBin ('F', 'B') rowMarker "" >>= readBin
        maybeCol = createBin ('L', 'R') columnMarker "" >>= readBin
    in (,) <$> maybeRow <*> maybeCol


createBin :: (Char, Char) -> String -> String -> Maybe String
createBin _ [] bin = Just bin
createBin opts@(zero, one) (c:cs) bin
    | c == zero = createBin opts cs $ bin ++ "0"
    | c == one = createBin opts cs $ bin ++ "1"
    | otherwise = Nothing


-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: String -> Maybe Int
readBin = fmap fst . Maybe.listToMaybe . Numeric.readInt 2 (`elem` "01") Char.digitToInt

