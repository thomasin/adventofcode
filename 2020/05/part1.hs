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
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Control.Applicative as Appl


data BoardingPass = BoardingPass
    { row :: Int
    , column :: Int
    , passId :: Int
    } deriving (Show)


main :: IO ()
main = do
    input <- getContents
    let boardingPasses = Maybe.mapMaybe attemptBoardingPass . lines $ input
    let maximumId = List.maximumBy (Ord.comparing passId) boardingPasses
    -- let boardingPasses = map boardingPass ["LLR", "RRR", "LRL"]
    print maximumId


attemptBoardingPass :: String -> Maybe BoardingPass
attemptBoardingPass possiblePass =
    let (rowMarker, columnMarker) = splitAt 7 possiblePass
        maybeRow = fromBinary "FB" rowMarker
        maybeCol = fromBinary "LR" columnMarker
    in Appl.liftA2 createBoardingPass maybeRow maybeCol


createBoardingPass :: Int -> Int -> BoardingPass
createBoardingPass r c = BoardingPass r c (r * 8 + c)


fromBinary :: String -> String -> Maybe Int
fromBinary digits@(_:_) =
    let ind c = Maybe.fromMaybe 0 $ List.elemIndex c digits
    in matchBinary . Numeric.readInt 2 (`elem` digits) ind
fromBinary _ = const Nothing


matchBinary :: [(Int, String)] -> Maybe Int
matchBinary ((x, _):[]) = Just x
matchBinary _ = Nothing

