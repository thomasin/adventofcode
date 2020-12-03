import Data.Map.Lazy (toList, fromListWith)

main = do     
    contents <- getContents
    print . fromListWith (+) . concatMap countChars $ lines contents

countChars :: String -> [(Int, Int)]
countChars string =
  (\(two, three) -> [(2, two), (3, three)])
  . findTwosAndThrees (0, 0)
  . toList
  $ fromListWith (+) [ (x, 1) | x <- string ]

findTwosAndThrees :: (Int, Int) -> [(Char, Int)] -> (Int, Int)
findTwosAndThrees (1, 1) _ = (1, 1)
findTwosAndThrees (two, three) [] = (two, three)
findTwosAndThrees (two, three) ((_, 2):xs) = findTwosAndThrees (1, three) xs
findTwosAndThrees (two, three) ((_, 3):xs) = findTwosAndThrees (two, 1) xs
findTwosAndThrees (two, three) (_:xs) = findTwosAndThrees (two, three) xs
