{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base matrix"
 --ghc-options=-Wall
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Data.Matrix
import Data.List
import Data.Maybe
-- import Data.Foldable
-- import Data.Functor.Compose


type Seats = Matrix Char


main :: IO ()
main = do
    input <- lines <$> getContents
    let seats = fromLists input :: Seats
    print $ choose $ choose seats


choose :: Seats -> Seats
choose seats = mapPos (decide seats) seats


decide :: Seats -> (Int, Int) -> Char -> Char
decide seats pos seat =
    case (seat, neighbours seats seat pos) of
        ('#', n) -> if (length $ filter occupied n) >= 4 then 'L' else '#'
        ('L', n) -> if (length $ filter occupied n) == 0 then '#' else 'L'
        (s, _) -> s


occupied = (==) '#'


neighbours :: Seats -> Char -> (Int, Int) -> [Char]
neighbours seats seat (row, col) =
    let startingRow = max 1 (row - 1)
        endingRow = min (nrows seats) (row + 1)
        startingCol = max 1 (col - 1)
        endingCol = min (ncols seats) (col + 1)
        subm = submatrix startingRow endingRow startingCol endingCol seats
    in delete seat . toList $ subm
