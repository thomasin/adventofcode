{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base containers"
 --ghc-options=-Wall
-}


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type Grid = Map (Int, Int, Int) Bool


main :: IO ()
main = do
    input <- getContents
    let grid = go 6 <$> fromString (0,0,0) Map.empty input
    print $ length <$> filter snd <$> Map.toList <$> grid


-- go


go :: Int -> Grid -> Grid
go 0 grid = grid
go count grid =
    go (count - 1) (refresh grid $ reachable grid)


refresh :: Grid -> Set (Int, Int, Int) -> Grid
refresh grid refreshable =
    Set.foldl
        (\a c -> Map.insert c (switch c grid) a)
        Map.empty
        refreshable


switch :: (Int, Int, Int) -> Grid -> Bool
switch coordinates grid =
    case (find coordinates grid, length $ filter (flip find grid) (neighbours coordinates)) of
        (True, 2) -> True
        (True, 3) -> True
        (True, _) -> False
        (False, 3) -> True
        (False, _) -> False


-- generation


reachable :: Grid -> Set (Int, Int, Int)
reachable grid =
    Map.foldrWithKey
        (\k _ s -> Set.union s $ Set.fromList $ neighbours k)
        Set.empty grid


fromString :: (Int, Int, Int) -> Grid -> String -> Maybe Grid
fromString _ grid [] = Just grid
fromString (x, y, z) grid (c:cs) =
    case c of
        '.' -> fromString (x + 1, y, z) (new (x, y, z) False grid) cs
        '#' -> fromString (x + 1, y, z) (new (x, y, z) True grid) cs
        '\n' -> fromString (0, y + 1, z) grid cs
        _ -> Nothing


new :: (Int, Int, Int) -> Bool -> Grid -> Grid
new coordinates isActive grid =
    Map.insert coordinates isActive grid


find :: (Int, Int, Int) -> Grid -> Bool
find coordinates grid =
    Map.findWithDefault False coordinates grid


neighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbours (x, y, z) =
    [ (x + 1, y, z)
    , (x - 1, y, z)
    , (x, y + 1, z)
    , (x, y - 1, z)
    , (x, y, z + 1)
    , (x, y, z - 1)
    , (x + 1, y + 1, z)
    , (x + 1, y - 1, z)
    , (x - 1, y + 1, z)
    , (x - 1, y - 1, z)
    , (x, y + 1, z + 1)
    , (x, y + 1, z - 1)
    , (x, y - 1, z + 1)
    , (x, y - 1, z - 1)
    , (x + 1, y, z + 1)
    , (x - 1, y, z + 1)
    , (x + 1, y, z - 1)
    , (x - 1, y, z - 1)
    , (x + 1, y + 1, z + 1)
    , (x + 1, y + 1, z - 1)
    , (x + 1, y - 1, z + 1)
    , (x + 1, y - 1, z - 1)
    , (x - 1, y + 1, z + 1)
    , (x - 1, y + 1, z - 1)
    , (x - 1, y - 1, z + 1)
    , (x - 1, y - 1, z - 1)
    ]
