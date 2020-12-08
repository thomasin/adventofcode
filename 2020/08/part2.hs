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
import qualified Data.Vector.Unboxed as Unboxed


type Instruction = (Bool, Char, Int)
type Instructions = Vector Instruction


main :: IO ()
main = do
    input <- lines <$> getContents
    let instructions = fromList $ mapMaybe (instruction . words) input :: Instructions
    print $ followInstructions 0 instructions 0 False


followInstructions :: Int -> Instructions -> Int -> Bool -> Maybe Int
followInstructions accumulator instructions index flipped =
    case instructions !? index of
          Just (True, _, _) -> Nothing -- Has already been visited, stumbled across an infinite loop

          Just c@(_, 'a', amount) ->
                followInstructions (accumulator + amount) (visit index c instructions) (index + 1) flipped -- Don't need to change this

          Just c@(_, 'n', _) -> -- First try original instruction, then try flipping it
                try flipped (followInstructions accumulator (visit index c instructions) (index + 1))
                            (followInstructions accumulator (swapTo index 'j' c instructions) index)

          Just c@(_, 'j', amount) -> -- First try original instruction, then try flipping it
                try flipped (followInstructions accumulator (visit index c instructions) (index + amount))
                            (followInstructions accumulator (swapTo index 'n' c instructions) index)

          Nothing -> if index == Unboxed.length instructions then Just accumulator else Nothing -- Either terminated or went too far

          _ -> Nothing -- Invalid instruction


try :: Bool -> (Bool -> Maybe Int) -> (Bool -> Maybe Int) -> Maybe Int
try True a _ = a True
try False a b =
  case a False of
    Nothing -> b True
    result -> result


visit :: Int -> Instruction -> Instructions -> Instructions
visit index (_, c, a) instructions = instructions // [(index, (True, c, a))]


swapTo :: Int -> Char -> Instruction -> Instructions -> Instructions
swapTo index char (_, _, a) instructions = instructions // [(index, (False, char, a))]


instruction :: [String] -> Maybe Instruction
instruction ((c:_):('+':num):[]) = (,,) False c <$> readMaybe num
instruction ((c:_):num:[]) = (,,) False c <$> readMaybe num
instruction _ = Nothing

