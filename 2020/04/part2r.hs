{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "megaparsec base text"
 --ghc-options=-Wall
-}

import Control.Monad
import Data.Void
import Data.Char
import Data.Maybe
import qualified Text.Read as Read
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Applicative.Permutations


type Parser = Parsec Void String


data Passport = Passport
    { birthYear :: Int
    , issueYear :: Int
    , expirationYear :: Int
    , height :: Height
    , hairColour :: String
    , eyeColour :: String
    , passportID :: String
    , countryID :: Maybe String
    } deriving (Show)

data HeightUnit = Cm | In deriving (Show)
data Height = Height HeightUnit Int deriving (Show)


main :: IO ()
main = do
    input <- getContents
    let result = parse parser "" input
    print $ either errorBundlePretty (show . length . catMaybes) result


parser :: Parser [Maybe Passport]
parser = do
    result <- possiblePassport `sepBy` blankLine
    eof
    return result


-- Parses a potential passport
possiblePassport :: Parser (Maybe Passport)
possiblePassport = do
    passport <- observing passportPermutations
    return $ either (const Nothing) id passport


passportPermutations :: Parser (Maybe Passport)
passportPermutations = intercalateEffect spaceChar $
    makePassport <$> toPermutation (passportField "byr")
                 <*> toPermutation (passportField "iyr")
                 <*> toPermutation (passportField "eyr")
                 <*> toPermutation (passportField "hgt")
                 <*> toPermutation (passportField "hcl")
                 <*> toPermutation (passportField "ecl")
                 <*> toPermutation (passportField "pid")
                 <*> toPermutationWithDefault Nothing (Just <$> passportField "cid")


makePassport :: String -> String -> String -> String -> String -> String -> String -> Maybe String -> Maybe Passport
makePassport byr iyr eyr hgt hcl ecl pid cid =
    let
        maybeBirthYear = toBirthYear byr
        maybeIssueYear = toIssueYear iyr
        maybeExpirationYear = toExpirationYear eyr
        maybeHeight = toHeight hgt
        maybeHairColour = toHairColour hcl
        maybeEyeColour = toEyeColour ecl
        maybePassportId = toPassportId pid
        maybeCountryId = Just cid
    in
    Passport <$> maybeBirthYear
             <*> maybeIssueYear
             <*> maybeExpirationYear
             <*> maybeHeight
             <*> maybeHairColour
             <*> maybeEyeColour
             <*> maybePassportId
             <*> maybeCountryId


passportField :: String -> Parser String
passportField fieldName = do
    void $ string fieldName
    void $ char ':'
    field <- takeWhileP (Just "not a space") (not . isSpace)
    return field


-- Parses a blank lineb
blankLine :: Parser ()
blankLine = do
    void hspace
    void newline
    return ()


-- Validators. Implementing this fully you would lose some type safety
-- which you can get back by creating unvalidated types for each fields.

ensure :: (a -> Bool) -> a -> Maybe a
ensure f a = if f a then Just a else Nothing

inRange :: Int -> Int -> Int -> Bool
inRange atLeast atMost i =
    i >= atLeast && i <= atMost


toBirthYear :: String -> Maybe Int
toBirthYear str =
    Read.readMaybe str >>= ensure (inRange 1920 2002)


toIssueYear :: String -> Maybe Int
toIssueYear str =
    Read.readMaybe str >>= ensure (inRange 2010 2020)


toExpirationYear :: String -> Maybe Int
toExpirationYear str = 
    Read.readMaybe str >>= ensure (inRange 2020 2030)


toHeight :: String -> Maybe Height
toHeight = parseMaybe heightParser

heightParser :: Parser Height
heightParser = do
    hStr <- some digitChar
    unit <- choice [ Cm <$ string "cm", In <$ string "in" ]

    let validateHeight atLeast atMost n h =
            if inRange atLeast atMost n then return h else fail "out of range"

    case Height unit <$> Read.readMaybe hStr of
            Nothing -> fail "invalid height"
            Just h@(Height Cm n) -> validateHeight 150 193 n h
            Just h@(Height In n) -> validateHeight 59 76 n h


toHairColour :: String -> Maybe String
toHairColour = parseMaybe hairColourParser

hairColourParser :: Parser String
hairColourParser = do 
    leadingHash <- char '#'
    colour <- some hexDigitChar
    guard (length colour == 6)
    return $ leadingHash:colour


toEyeColour :: String -> Maybe String
toEyeColour = ensure (\s -> s `elem` eyeColours)

eyeColours :: [String]
eyeColours =
    [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]


toPassportId :: String -> Maybe String
toPassportId = ensure ((==) 9 . length)






