{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base text vector"
 --ghc-options=-Wall
-}


import Data.Maybe (mapMaybe)
import Text.Read
import Data.Vector.Unboxed (Vector, update, singleton, fromList, toList, (!?), (//))


type Instructions = Vector (Bool, Char, Int)


main :: IO ()
main = do
    input <- lines <$> getContents
    let instructions = fromList $ mapMaybe (instruction . words) input :: Instructions
    print $ followInstructions 0 instructions 0


followInstructions :: Int -> Instructions -> Int -> Maybe Int
followInstructions accumulator instructions index =
    case instructions !? index of
          Just (True, _, _) -> Just accumulator -- Has already been visited
          Just c@(_, 'n', _) -> followInstructions accumulator (visit index c instructions) (index + 1) -- No-op
          Just c@(_, 'a', amount) -> followInstructions (accumulator + amount) (visit index c instructions) (index + 1)
          Just c@(_, 'j', amount) -> followInstructions accumulator (visit index c instructions) (index + amount)
          _ -> Nothing -- Either index does not exist, or came across an invalid instruction


visit :: Int -> (Bool, Char, Int) -> Instructions -> Instructions
visit index (_, c, a) instructions =
    instructions // [(index, (True, c, a))]


instruction :: [String] -> Maybe (Bool, Char, Int)
instruction ((c:_):int:[]) = (,,) False c <$> signedInt int
instruction _ = Nothing


signedInt :: String -> Maybe Int
signedInt ('+':num) = readMaybe num
signedInt num = readMaybe num
