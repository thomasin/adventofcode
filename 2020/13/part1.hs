{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "base megaparsec"
 --ghc-options=-Wall
-}


import Control.Monad
import Data.Maybe
import Text.Read
import Data.Void
import Data.List
import Data.Char
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String


main :: IO ()
main = do
    input <- getContents
    case parseMaybe parser input of
        Nothing -> print "failed to parse input"
        Just schedule -> print $ uncurry (*) $ findBus $ schedule


-- on the assumption there will never be a 0 minute wait lol
findBus :: (Int, [Int]) -> (Int, Int)
findBus (departing, numbers) =
    let minutes = zip numbers $ map (\n -> n - mod departing n) numbers
    in minimumBy (comparing snd) minutes


-- parser

parser :: Parser (Int, [Int])
parser = do
    departing <- maybeInt >>= maybe (fail "int") return
    void $ newline
    numbers <- catMaybes <$> maybeInt `sepBy` char ','
    void $ eof
    return (departing, numbers)


maybeInt :: Parser (Maybe Int)
maybeInt = readMaybe <$> takeWhileP Nothing isAlphaNum







