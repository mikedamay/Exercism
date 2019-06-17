module FoodChain (song) where

import qualified Data.Map as Map
import Data.Char (toLower)
import Data.List (intercalate)

eaten = ["fly", "spider that wriggled and jiggled and tickled inside her", "bird", "cat", "dog", "goat", "cow", "horse"]

lyrics = [
        (VerseStart, "I know an old lady who swallowed a {{animal}}.\n")
        ,(VerseEnd, "I don't know why she swallowed the fly. Perhaps she'll die.\n")
        ,(VerseContinuation, "She swallowed the {{eater}} to catch the {{eaten}}.\n")
        ,(SongEnd, "She's dead, of course!\n")
        ,(AnimalQuirk Spider, "It wriggled and jiggled and tickled inside her.\n")
        ,(AnimalQuirk Bird, "How absurd to swallow a bird!\n")
        ,(AnimalQuirk Cat, "Imagine that, to swallow a cat!\n")
        ,(AnimalQuirk Dog, "What a hog, to swallow a dog!\n")
        ,(AnimalQuirk Goat, "Just opened her throat and swallowed a goat!\n")
        ,(AnimalQuirk Cow, "I don't know how she swallowed a cow!\n")
    ]

lyricMap :: Map.Map LineRole String
lyricMap = Map.fromList lyrics


data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse deriving (Show, Eq, Ord, Enum)

allAnimals :: [Animal]
allAnimals = take numAllAnimals $ iterate succ Fly

numAnimalsInRange :: Animal -> Animal -> Int
numAnimalsInRange from to = (fromEnum to) - (fromEnum from) + 1

numAllAnimals :: Int
numAllAnimals = numAnimalsInRange Fly Horse

data LineRole = VerseStart | VerseEnd | VerseContinuation | SongEnd | FirstSpider
     | AnimalQuirk Animal deriving (Show, Eq, Ord)

data Verse = FirstVerse LineRole LineRole
            | SecondVerse LineRole LineRole LineRole LineRole
            | MainVerse LineRole LineRole [LineRole] LineRole
            | LastVerse LineRole LineRole
            deriving (Show)


song :: String
song = intercalate "\n" $ structureToText songStructure

eatenMap :: Map.Map Animal String
eatenMap = Map.fromList $ zip allAnimals eaten

eaterMap :: Map.Map Animal String
eaterMap = Map.fromList $ zip allAnimals $ map (map toLower) (map show allAnimals)

songStructure :: [Verse]
songStructure = [FirstVerse VerseStart VerseEnd]
                ++ [SecondVerse VerseStart (AnimalQuirk Spider) VerseContinuation VerseEnd]
                ++  [
                    (MainVerse
                        VerseStart
                        (AnimalQuirk a)
                        (replicateForEachAnimal a VerseContinuation)
                        VerseEnd) | a <- [Bird, Cat, Dog, Goat, Cow]]
                ++ [LastVerse VerseStart SongEnd]

replicateForEachAnimal :: Animal -> a -> [a]
replicateForEachAnimal animal = replicate (numAnimalsInRange Spider animal)

getText :: (Verse, Animal) -> String
getText ((FirstVerse a b), animal) = (replace "{{animal}}" (animalToText animal) $ lookupLyric a) ++ (lookupLyric b)
getText ((SecondVerse a b c d), animal) = (replace "{{animal}}" (animalToText animal) $ lookupLyric a) ++ (lookupLyric b) ++ (showLyric2 (animal, c)) ++ (lookupLyric d)
getText ((MainVerse a b c d), animal) = generateMainVerse a b c d animal
getText ((LastVerse a b), animal) = (replace "{{animal}}" (animalToText animal) $ lookupLyric a) ++ (lookupLyric b)


replace _ _ [] = []
replace pattern substitute xs
    | (take len xs) == pattern = substitute ++ (replace pattern substitute $ drop len xs)
    | otherwise = head xs : (replace pattern substitute (tail xs))
    where len = length pattern

structureToText :: [Verse] -> [String]
structureToText structure = map getText $ zip structure allAnimals

generateMainVerse :: LineRole -> LineRole -> [LineRole] -> LineRole -> Animal -> String
generateMainVerse start quirk continuations end animal =
    (replace "{{animal}}" (animalToText animal) $ lookupLyric start) ++ (lookupLyric quirk) ++ (generateContinuationLines continuations animal) ++ (lookupLyric end)

generateContinuationLines :: [LineRole] -> Animal -> String
generateContinuationLines continuations animal =
    concat $ map showLyric lines
    where lines = zip (iterate pred animal) continuations

showLyric :: (Animal, LineRole) -> String
showLyric (a, l) = (replace "{{eater}}" (lookupEater a)) $ (replace "{{eaten}}" $ lookupEaten $ pred a) $ (lookupLyric l)

showLyric2 :: (Animal, LineRole) -> String
showLyric2 (a, l) = (replace "{{eater}}" (lookupEater a)) $ (replace "{{eaten}}" $ (map toLower $ show $ pred a)) $ (lookupLyric l)

lookupLyric :: LineRole -> String
lookupLyric lr = dropit (Map.lookup lr lyricMap)

lookupEaten :: Animal -> String
lookupEaten a = dropit (Map.lookup a eatenMap )

lookupEater :: Animal -> String
lookupEater a = dropit (Map.lookup a eaterMap )

dropit :: Maybe String -> String
dropit (Just x) = x
dropit Nothing = error "corrupt embedded data"

animalToText :: Animal -> String
animalToText a = map toLower $ show a



