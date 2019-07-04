module House (rhyme) where

import Data.List (intersperse)

rhymex :: String
rhymex = "This is the house that Jack built.\n\
        \\n\
        \This is the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the farmer sowing his corn\n\
        \that kept the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the horse and the hound and the horn\n\
        \that belonged to the farmer sowing his corn\n\
        \that kept the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n"

this_is :: String
this_is = "This is"
that :: String
that = "that"
num_players :: Int
num_players = length players
max_player :: Int
max_player = length players - 1


players :: [(String, String)]
{-
players = [
        ("ate", "the malt")
        ,("lay in", "the house that Jack built.")
        ]
-}
players = [
        ("", "the horse and the hound and the horn")
        ,("belonged to", "the farmer sowing his corn")
        ,("kept", "the rooster that crowed in the morn")
        ,("woke", "the priest all shaven and shorn")
        ,("married", "the man all tattered and torn")
        ,("kissed", "the maiden all forlorn")
        ,("milked", "the cow with the crumpled horn")
        ,("tossed", "the dog")
        ,("worried", "the cat")
        ,("killed", "the rat")
        ,("ate", "the malt")
        ,("lay in", "the house that Jack built.")
        ]



rhyme :: [Char]
rhyme = concat $ concat $ intersperse ["\n"] $ map (\x -> [mkLine idx y | (idx, y) <- zip [0..] (drop (max_player - x) players)]) [0..max_player]

mkLine :: Int -> (String, String) -> String
mkLine 0 (_, player) = this_is ++ " " ++ player ++ "\n"
mkLine _ (action, player) = glue that action player

glue :: String -> String -> String -> String
glue prefix action player = (concat $ intersperse " " [prefix, action, player]) ++ "\n"