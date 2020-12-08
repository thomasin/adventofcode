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
import Data.Vector.Unboxed (Vector, fromList, (!?), (//))


type Instruction = (Bool, Char, Int)
type Instructions = Vector Instruction


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


visit :: Int -> Instruction -> Instructions -> Instructions
visit index (_, c, a) instructions = instructions // [(index, (True, c, a))]


instruction :: [String] -> Maybe Instruction
instruction ((c:_):('+':num):[]) = (,,) False c <$> readMaybe num
instruction ((c:_):num:[]) = (,,) False c <$> readMaybe num
instruction _ = Nothing
