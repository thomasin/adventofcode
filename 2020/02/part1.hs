{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "megaparsec base"
 --ghc-options=-Wall
-}

import Control.Applicative
import Control.Monad
import Data.Void
import Data.Char
import Data.List
import Data.Maybe
import Text.Read
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


type Password = Maybe String


main :: IO ()
main = do
    input <- getContents
    let result = parse parser "" input
    print (either errorBundlePretty (show . length . catMaybes) result)


parser :: Parser [Maybe String]
parser = do
    result <- possiblePassword `sepBy` newline
    eof
    return result


possiblePassword :: Parser (Maybe String)
possiblePassword = do
    config <- passwordConfig
    password <- takeWhileP (Just "password character") isLower

    if passwordIsValid config password then
        return $ Just password
    else
        return Nothing


passwordIsValid :: (Int, Int, Char) -> String -> Bool
passwordIsValid (minOccurences, maxOccurences, keyChar) password =
    let keyCharOccurences = length $ filter ((==) keyChar) password
    in keyCharOccurences >= minOccurences && keyCharOccurences <= maxOccurences


passwordConfig :: Parser (Int, Int, Char)
passwordConfig = do
    minOccurences <- integer
    void $ char '-'
    maxOccurences <- integer
    void $ space
    keyChar <- lowerChar
    void $ char ':'
    void $ space
    return (minOccurences, maxOccurences, keyChar)


integer :: Parser Int
integer = do
    intStr <- Text.Megaparsec.some digitChar
    case readMaybe intStr of Nothing -> fail "not an int"
                             Just i -> return i

