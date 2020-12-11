{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base grids"
 --ghc-options=-Wall
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Data.Grid
import Data.Foldable
import Data.Functor.Compose


type Seats = Grid '[98,96] Char


main :: IO ()
main = do
    input <- lines <$> getContents
    let seats = fromNestedLists input :: Maybe Seats
    print $ occupied <$> toList <$> start <$> seats


start :: Seats -> Seats
start seats =
    let chosen = choose seats
    in if chosen == seats then chosen else start chosen


choose :: Seats -> Seats
choose = autoConvolute @[3, 3] omitBounds rule


rule :: Compose (Grid '[3, 3]) Maybe Char -> Char
rule (Compose window') =
    let (seat, mn) = partitionFocus window'
    in case (seat, occupied $ neighbours mn) of
            (Just '#', count) | count >= 4 -> 'L'
            (Just 'L', count) | count == 0 -> '#'
            (Just s, _) -> s
            _ -> '.'


occupied :: [Char] -> Int
occupied = length . filter ((==) '#')


neighbours :: Grid dim (Maybe (Maybe Char)) -> [Char]
neighbours = toList . Compose . Compose