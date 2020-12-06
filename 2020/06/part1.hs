{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base split"
 --ghc-options=-Wall
-}


import qualified Data.List as List
import qualified Data.List.Split as Split


main :: IO ()
main = do
    input <- getContents
    let groups = map lines $ Split.splitOn "\n\n" input
    print $ sum $ map (length . questionsAnswered) groups


questionsAnswered :: [String] -> String
questionsAnswered = List.nub . List.concat

