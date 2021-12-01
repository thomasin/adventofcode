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
    print (windowedSum numbersList 0)


windowedSum :: [Int] -> Int -> Int
windowedSum (x1:x2:x3:x4:xs) count
    | x1 < x4 = windowedSum (x2:x3:x4:xs) (count + 1)
    | otherwise = windowedSum (x2:x3:x4:xs) count
windowedSum _ count = count
