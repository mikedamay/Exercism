module FoodChain (song) where

import qualified Data.Map as Map

prey = ["fly", "spider that wriggled and jiggled and tickled inside her", "bird", "cat", "dog", "goat", "cow", "horse"]

data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse deriving (Show, Eq, Ord, Enum)
data LineRole = VerseStart | VerseEnd | VerseContinuation | SongEnd1 | SongEnd2 | FirstSpider
     | AnimalQuirk Animal deriving (Show, Eq, Ord)
data Verse = FirstVerse LineRole LineRole
            | SecondVerse LineRole LineRole LineRole LineRole
            | MainVerse LineRole LineRole [LineRole] LineRole
            | LastVerse LineRole LineRole
            deriving (Show)

eatenMap :: Map.Map Animal String
eatenMap = Map.fromList $ zip (take ((fromEnum Horse) - (fromEnum Fly) + 1) $ iterate succ Fly) prey

firstVerse :: (Verse, Animal)
firstVerse = (FirstVerse VerseStart VerseEnd, Fly)

secondVerse :: (Verse, Animal)
secondVerse = (SecondVerse VerseStart (AnimalQuirk Spider) VerseContinuation VerseEnd, Spider)

lastVerse :: (Verse, Animal)
lastVerse = (LastVerse SongEnd1 SongEnd2, Horse)

middleVerses :: [(Verse, Animal)]
middleVerses = map hydrate $ take 5 $ iterate succ Bird
    where hydrate a = (MainVerse VerseStart (AnimalQuirk a) (replicate ((fromEnum a) - (fromEnum Bird) + 1) VerseContinuation) VerseEnd, a)

lyricMap :: Map.Map LineRole String
lyricMap = Map.fromList lyrics

songStructure = [firstVerse, secondVerse] ++ middleVerses ++ [lastVerse]

getText :: (Verse, Animal) -> Maybe String
getText ((FirstVerse a b), _) = (Map.lookup a lyricMap) `myJoin` (Map.lookup b lyricMap)
getText ((SecondVerse a b c d), _) = (Map.lookup a lyricMap) `myJoin` (Map.lookup a lyricMap) `myJoin` (Map.lookup c lyricMap) `myJoin` (Map.lookup d lyricMap)
getText ((MainVerse a b c d), animal) = generateMainVerse a b c d animal
getText ((LastVerse a b), _) = (Map.lookup a lyricMap) `myJoin` (Map.lookup b lyricMap)

myJoin :: Maybe String -> Maybe String -> Maybe String
myJoin Nothing _ = Nothing
myJoin _ Nothing = Nothing
myJoin (Just a) (Just b) = Just (a ++ b)

lyrics = [
        (VerseStart, "I know an old lady who swallowed a {{animal}}.\n")
        ,(VerseEnd, "I don't know why she swallowed the fly. Perhaps she'll die.\n")
        ,(VerseContinuation, "She swallowed the {{eater}} to catch the {{eaten}}.\n")
        ,(SongEnd1, "I know an old lady who swallowed a horse.\n")
        ,(SongEnd2, "She's dead, of course!\n")
        ,(AnimalQuirk Spider, "It wriggled and jiggled and tickled inside her.\n")
        ,(AnimalQuirk Bird, "How absurd to swallow a bird!\n")
        ,(AnimalQuirk Cat, "Imagine that, to swallow a cat!\n")
        ,(AnimalQuirk Dog, "What a hog, to swallow a dog!\n")
        ,(AnimalQuirk Goat, "Just opened her throat and swallowed a goat!\n")
        ,(AnimalQuirk Cow, "I don't know how she swallowed a cow!\n")
    ]

replace _ _ [] = []
replace pattern substitute xs
    | (take len xs) == pattern = substitute ++ (replace pattern substitute $ drop len xs)
    | otherwise = head xs : (replace pattern substitute (tail xs))
    where len = length pattern

structureToText :: [(Verse, Animal)] -> [Maybe String]
structureToText = map getText

generateMainVerse :: LineRole -> LineRole -> [LineRole] -> LineRole -> Animal -> Maybe String
generateMainVerse start quirk continuations end animal =
    (Map.lookup start lyricMap) `myJoin` (Map.lookup quirk lyricMap) `myJoin` (generateContinuationLines continuations animal) `myJoin` (Map.lookup end lyricMap)

generateContinuationLines :: [LineRole] -> Animal -> Maybe String
generateContinuationLines continuations animal =
    Just (concat $ map showLyric lines)
    where lines = zip (reverse $ iterate succ Bird) continuations

showLyric :: (Animal, LineRole) -> String
showLyric (a, l) = (replace "{{eater}}" (lookupEaten a)) $ (replace "{{eaten}}" $ lookupEaten $ pred a) $ (lookupLyric l)

numAnimals :: Animal -> Animal -> Int
numAnimals g l = (fromEnum g) - (fromEnum l) + 1

lookupLyric :: LineRole -> String
lookupLyric lr = dropit (Map.lookup lr lyricMap)

lookupEaten :: Animal -> String
lookupEaten a = dropit (Map.lookup a eatenMap )

dropit :: Maybe String -> String
dropit (Just x) = x
dropit Nothing = error "corrupt embedded data"


song :: String
song =
    "I know an old lady who swallowed a fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a spider.\n\
    \It wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a bird.\n\
    \How absurd to swallow a bird!\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a cat.\n\
    \Imagine that, to swallow a cat!\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a dog.\n\
    \What a hog, to swallow a dog!\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a goat.\n\
    \Just opened her throat and swallowed a goat!\n\
    \She swallowed the goat to catch the dog.\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a cow.\n\
    \I don't know how she swallowed a cow!\n\
    \She swallowed the cow to catch the goat.\n\
    \She swallowed the goat to catch the dog.\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a horse.\n\
    \She's dead, of course!\n"


