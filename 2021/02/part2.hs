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
    print (navigate (words input) (0, 0, 0))


navigate :: [String] -> (Int, Int, Int) -> Int
navigate (command:amount:xs) (horizontal, depth, aim) =
    case command of
        "forward" ->
            navigate xs (horizontal + read amount, depth + (aim * read amount), aim)

        "up" ->
            navigate xs (horizontal, depth, aim - read amount)

        "down" ->
            navigate xs (horizontal, depth, aim + read amount)

        _ ->
            navigate xs (horizontal, depth, aim)

navigate _ (horizontal, depth, _) = horizontal * depth
