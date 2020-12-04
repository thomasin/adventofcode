{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "megaparsec base text parser-combinators"
 --ghc-options=-Wall
-}


import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Char
import Data.Maybe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Applicative.Permutations


type Parser = Parsec Void String


main :: IO ()
main = do
    input <- getContents
    let result = parse parser "" input
    print (either errorBundlePretty (show . length . catMaybes) result)


parser :: Parser [Maybe Passport]
parser = do
    result <- possiblePassport `sepBy` blankLine
    eof
    return result


data Passport = Passport
    { birthYear :: String
    , issueYear :: String
    , expirationYear :: String
    , height :: String
    , hairColor :: String
    , eyeColor :: String
    , passportID :: String
    , countryID :: Maybe String
    } deriving (Show)


-- Parses a potential passport
possiblePassport :: Parser (Maybe Passport)
possiblePassport = do
    passport <- observing passportPermutations
    -- return passport
    case passport of
        Left _ -> return Nothing
        Right p -> return (Just p)


passportPermutations = intercalateEffect spaceChar $
    makePassport <$> toPermutation (passportField "byr")
                 <*> toPermutation (passportField "iyr")
                 <*> toPermutation (passportField "eyr")
                 <*> toPermutation (passportField "hgt")
                 <*> toPermutation (passportField "hcl")
                 <*> toPermutation (passportField "ecl")
                 <*> toPermutation (passportField "pid")
                 <*> toPermutationWithDefault Nothing (fmap Just $ passportField "cid")
    where
        makePassport byr iyr eyr hgt hcl ecl pid cid =
            Passport byr iyr eyr hgt hcl ecl pid cid


passportField :: String -> Parser String
passportField fieldName = do
    void(string fieldName)
    void(char ':')
    field <- takeWhile1P (Just "passport field value") (not . isSpace)
    return field


-- Parses a blank line
blankLine :: Parser ()
blankLine = do
    void(hspace)
    void(newline)
    return ()

