module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Empty | Head a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum Empty = error "empty list"
datum (Head h t) = h

fromList :: [a] -> LinkedList a
fromList [] = Empty
fromList (x:xs) = Head x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
-- new x Empty = Head x Empty
new x ll = Head x ll

next :: LinkedList a -> LinkedList a
next Empty = Empty
next (Head h t) = t

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList ll = reversell ll Empty

reversell :: LinkedList a -> LinkedList a -> LinkedList a
reversell Empty reversed = reversed
reversell (Head h1 t1) reversed = reversell t1 (Head h1 reversed)

toList :: LinkedList a -> [a]
toList Empty = []
toList (Head h t) = h : (toList t)
