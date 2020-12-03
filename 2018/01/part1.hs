import Text.Read
import Data.Maybe (catMaybes)

main = do     
    contents <- getContents
    print (sum $ stringToNumbers contents)

stringToNumbers :: String -> [Int] 
stringToNumbers =
    catMaybes . map signedInt . lines

signedInt :: String -> Maybe Int
signedInt (x:xs)
    | x == '+' = readMaybe xs
    | x == '-' = fmap (* (-1)) (readMaybe xs)
    | otherwise = Nothing
