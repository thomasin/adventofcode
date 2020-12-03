{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "matrix"
 --ghc-options=-Wall
-}


import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix


main :: IO ()
main = do
    input <- getContents
    let trees = Matrix.fromLists $ lines input
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    let multipliedTrees = foldl (\num slope -> (*) num $ sled trees slope 0 (1, 1)) 1 slopes
    print multipliedTrees


sled :: Matrix Char -> (Int, Int) -> Int -> (Int, Int) -> Int
sled trees slope treesSeen (row, col)
    | row > Matrix.nrows trees = treesSeen -- Reached the bottom
    | otherwise = sled trees slope (checkTree treesSeen $ getTree trees row col) (moveBy slope (row, col))


getTree :: Matrix Char -> Int -> Int -> Char
getTree trees row col =
    let transformedCol = ((col - 1) `mod` Matrix.ncols trees) + 1 -- -1 when transforming so we can add one at the end and avoid 0
    in Matrix.getElem row transformedCol trees


checkTree :: Int -> Char -> Int
checkTree treesSeen '#' = treesSeen + 1
checkTree treesSeen _ = treesSeen 


moveBy :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveBy (right, down) (row, col) =
    (row + down, col + right)