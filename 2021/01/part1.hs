{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "containers"
 --ghc-options=-Wall
-}


main :: IO ()
main = do
    input <- getContents
    let numbersList = map read $ lines $ input
    print (findIncreases numbersList 0)


findIncreases :: [Int] -> Int -> Int
findIncreases (x1:x2:xs) count
    | x1 < x2 = findIncreases (x2:xs) (count + 1)
    | otherwise = findIncreases (x2:xs) count
findIncreases _ count = count
