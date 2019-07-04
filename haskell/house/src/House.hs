module House (rhyme) where

import Data.List (intersperse)

this_is :: String
this_is = "This is"
that :: String
that = "that"
max_player :: Int
max_player = length players - 1


players :: [(String, String)]
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
rhyme = concat $ concat $ intersperse ["\n"] $ map (\n -> [mkLine idx player | (idx, player) <- zip [0..] (drop (max_player - n) players)]) [0..max_player]

mkLine :: Int -> (String, String) -> String
mkLine 0 (_, player) = this_is ++ " " ++ player ++ "\n"
mkLine _ (action, player) = (concat $ intersperse " " [that, action, player]) ++ "\n"
