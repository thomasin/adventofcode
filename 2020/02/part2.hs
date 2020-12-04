{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "megaparsec base"
 --ghc-options=-Wall
-}

import Control.Monad
import Data.Void
import Data.Char
import Data.Maybe
import Text.Read
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char


type Parser = Parsec Void String


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

    let valid = passwordIsValid config password
    if valid then return $ Just password else return Nothing


passwordIsValid :: (Int, Int, Char) -> String -> Bool
passwordIsValid (i1, i2, keyChar) password =
    let isKeyChar i c = i `elem` [i1, i2] && c == keyChar
        lookForKeyChar (i, exists) c = ( i + 1, if isKeyChar i c then not exists else exists )
    in snd $ foldl lookForKeyChar (1, False) password


passwordConfig :: Parser (Int, Int, Char)
passwordConfig = do
    i1 <- integer
    void $ char '-'
    i2 <- integer
    void $ space
    keyChar <- lowerChar
    void $ char ':'
    void $ space
    return (i1, i2, keyChar)


integer :: Parser Int
integer = do
    intStr <- Text.Megaparsec.some digitChar
    case readMaybe intStr of Nothing -> fail "not an int"
                             Just i -> return i

