import Text.Read (readMaybe)

main = do     
    contents <- getContents
    let numbers = stringsToNumbers $ lines contents
    print (findDuplicate [0] numbers numbers)

stringsToNumbers :: [String] -> [Int]
stringsToNumbers [] = []
stringsToNumbers (x:xs) =
    let next = stringsToNumbers xs
    in maybe next (:next) $ signedInt x

signedInt :: String -> Maybe Int
signedInt number@(x:xs) =
    readMaybe $ if x == '+' then xs else number

findDuplicate :: [Int] -> [Int] -> [Int] -> Maybe Int
findDuplicate [] _ _ = Nothing
findDuplicate results@(r:rs) intList (x:xs) =
    let result = r + x
        next = findDuplicate (result:results) intList
    in if elem result results
      then Just result
      else next $ if xs == [] then intList else xs
