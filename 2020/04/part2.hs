{- stack script
 --compile
 --copy-bins
 --resolver nightly-2020-11-28
 --install-ghc
 --package "megaparsec base text parser-combinators"
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


data Passport = Passport
    { birthYear :: Int
    , issueYear :: Int
    , expirationYear :: Int
    , height :: Height
    , hairColor :: String
    , eyeColor :: String
    , passportID :: String
    , countryID :: Maybe String
    }

data HeightUnit = Cm | In
data Height = Height Int HeightUnit


-- Parses a potential passport
possiblePassport :: Parser (Maybe Passport)
possiblePassport = do
    passport <- observing passportPermutations
    return $ either (const Nothing) id passport


-- I feel like there should be a way nicer way to write this logic
makePassport :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Height -> Maybe String -> Maybe String -> Maybe String -> Maybe (Maybe String) -> Maybe Passport
makePassport _ _ _ _ _ _ _ (Just Nothing) = Nothing
makePassport byr iyr eyr hgt hcl ecl pid Nothing =
    Passport <$> byr <*> iyr <*> eyr <*> hgt <*> hcl <*> ecl <*> pid <*> (Just Nothing)
makePassport byr iyr eyr hgt hcl ecl pid cid =
    Passport <$> byr <*> iyr <*> eyr <*> hgt <*> hcl <*> ecl <*> pid <*> cid


passportPermutations :: Parser (Maybe Passport)
passportPermutations = runPermutation $
    makePassport <$> required (passportField "byr" birthYearParser)
                 <*> required (passportField "iyr" issueYearParser)
                 <*> required (passportField "eyr" expirationYearParser)
                 <*> required (passportField "hgt" heightParser)
                 <*> required (passportField "hcl" hairColourParser)
                 <*> required (passportField "ecl" eyeColourParser)
                 <*> required (passportField "pid" passportIdParser)
                 <*> nothingable (passportField "cid" countryIdParser)


required :: Parser a -> Permutation Parser a
required = toPermutation

nothingable :: Parser a -> Permutation Parser (Maybe a)
nothingable = toPermutationWithDefault Nothing . fmap Just


passportField :: String -> Parser a -> Parser (Maybe a)
passportField fieldName fieldParser = do
    void $ string fieldName
    void $ char ':'
    field <- withRecovery throwAwayField (Just <$> fieldParser)
    void $ withRecovery throwAwayField (Just <$> spaceChar)
    return field


throwAwayField :: ParseError String Void -> Parser (Maybe a)
throwAwayField _ = do
    void anyField
    return Nothing


-- Parses a blank lineb
blankLine :: Parser ()
blankLine = do
    void hspace
    void newline
    return ()


-- Passport field parser helpers
anyField :: Parser String
anyField = takeWhileP (Just "not a space") (not . isSpace)

integer :: Parser Int
integer = do
    intStr <- Text.Megaparsec.some digitChar
    case Read.readMaybe intStr of Nothing -> fail "not an int"
                                  Just i -> return i

integerInRange :: Int -> Int -> Parser Int
integerInRange atLeast atMost = do
    int <- integer
    guard (int >= atLeast && int <= atMost) <?> "number in range"
    return $ int

stringWith :: Int -> Parser Char -> Parser String
stringWith n p = do
    str <- Text.Megaparsec.some p
    guard (length str == n)
    return str


-- Passport field parsers
birthYearParser :: Parser Int
birthYearParser = integerInRange 1920 2002

issueYearParser :: Parser Int
issueYearParser = integerInRange 2010 2020

expirationYearParser :: Parser Int
expirationYearParser = integerInRange 2020 2030

heightParser :: Parser Height
heightParser = do
    h <- integer
    unit <- choice [ Cm <$ string "cm", In <$ string "in" ]

    case unit of Cm -> guard (h >= 150 && h <= 193)
                 In -> guard (h >= 59 && h <= 76)

    return $ Height h unit

hairColourParser :: Parser String
hairColourParser = do
    leadingHash <- char '#'
    colour <- stringWith 6 hexDigitChar
    return $ leadingHash:colour

eyeColourParser :: Parser String
eyeColourParser = Text.Megaparsec.choice
    [ string "amb"
    , string "blu"
    , string "brn"
    , string "gry"
    , string "grn"
    , string "hzl"
    , string "oth"
    ]

passportIdParser :: Parser String
passportIdParser = stringWith 9 digitChar

countryIdParser :: Parser String
countryIdParser = anyField
