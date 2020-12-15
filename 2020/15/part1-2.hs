{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base containers split"
 --ghc-options=-Wall
-}


import Text.Read
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.IntMap as IntMap


type Check = IntMap.IntMap [Int]


main :: IO ()
main = do
    input <- mapMaybe readMaybe <$> splitOn "," <$> getContents
    let check = IntMap.fromList $ zip input $ group [1..]
    print $ turns check (length input + 1) (last input)


turns :: Check -> Int -> Int -> Int
turns _ 30000001 x = x
turns check turn x =
    let speak a = IntMap.insertWith (const ((:) turn)) a [turn] check
    in case IntMap.lookup x check of -- lookup 6
             Just (t1:t2:_) -> turns (speak (t1 - t2)) (turn + 1) (t1 - t2)
             _ -> turns (speak 0) (turn + 1) 0
