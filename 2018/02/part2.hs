import Data.List

main = do
    contents <- getContents
    -- print . perms 5 $ lines contents
    print . siblingIds . getIds $ lines contents

siblingIds :: [(String, String)] -> [Char]
siblingIds [] = []
siblingIds ((x, y):xys) =
  let (siblings, same) = compareIds x y 0 []
  in if siblings then reverse same
                 else siblingIds xys

getIds :: [String] -> [(String, String)]
getIds l = [(x,y) | (x:ys) <- tails l, y <- ys]

perms :: Int -> [a] -> [[a]]
perms _ [] = []
perms 0 list = [[]]
perms 1 list = map (:[]) list
perms n (x:xs) = (map (x:) $ perms (n-1) xs) ++ (perms n xs)

-- perms 2 [1..5] = (map 1: perms 1 [2..5]):[perms 2 [2..5]]
--                = (map 1: [[2], [3], [4], [5]])
--                = [[1, 2], [1, 3], [1, 4], [1, 5]] ++

compareIds :: String -> String -> Int -> [Char] -> (Bool, [Char])
compareIds _ _ 2 _ = (False, [])
compareIds [] _ 0 _ = (False, [])
compareIds _ [] 0 _ = (False, [])
compareIds _ [] _ s = (True, s)
compareIds [] _ _ s = (True, s)
compareIds (x:xs) (y:ys) n s =
  if x == y then compareIds xs ys n (x:s)
            else compareIds xs ys (n + 1) s
