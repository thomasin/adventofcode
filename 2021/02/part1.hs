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
    print (navigate (words input) (0, 0))


navigate :: [String] -> (Int, Int) -> Int
navigate (command:amount:xs) (horizontal, depth) =
    case command of
        "forward" ->
            navigate xs (horizontal + read amount, depth)

        "up" ->
            navigate xs (horizontal, depth - read amount)

        "down" ->
            navigate xs (horizontal, depth + read amount)

        _ ->
            navigate xs (horizontal, depth)

navigate _ (horizontal, depth) = horizontal * depth
