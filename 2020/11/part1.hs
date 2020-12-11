{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base grids"
 --ghc-options=-Wall
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Data.Grid
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Functor.Compose


type Seats = Grid '[10,10] Char


main :: IO ()
main = do
    input <- lines <$> getContents
    let seats = fromNestedLists input :: Maybe Seats
    print $ choose <$> choose <$> seats


choose :: Seats -> Seats
choose = autoConvolute @[3, 3] omitBounds rule


rule :: Compose (Grid '[3, 3]) Maybe Char -> Char
rule (Compose window') =
    case partitionFocus window' of
        (Just '#', neighbours) ->
            if countOccupied neighbours > 4 then 'L' else '#'

        (Just 'L', neighbours) ->
            if countOccupied neighbours == 0 then '#' else 'L'

        (Just seat, _) ->
            seat


countOccupied :: Grid [3, 3] (Maybe (Maybe Char)) -> Int
countOccupied neighbours =
    length . filter ((==) '#') . toList . Compose $ Compose neighbours
