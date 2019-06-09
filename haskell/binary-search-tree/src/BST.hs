module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where


-- data BST a = BST a (Maybe (BST a)) (Maybe (BST a)) deriving (Eq, Show)
data BST a = BST {bstValue :: Maybe a, bstLeft :: Maybe (BST a), bstRight :: Maybe (BST a) } | Empty deriving (Eq, Show)

-- bstLeft :: BST a -> Maybe (BST a)
-- bstLeft (BST _ Nothing _) = Nothing
--
-- bstRight :: BST a -> Maybe (BST a)
-- bstRight tree = error "You need to implement this function."

-- bstValue :: BST a -> Maybe a
-- bstValue tree = error "You need to implement this function."

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList xs = error "You need to implement this function."

insert :: Ord a => a -> BST a -> BST a
insert x tree = error "You need to implement this function."

singleton :: a -> BST a
singleton x = BST (Just x) Nothing Nothing

toList :: BST a -> [a]
toList tree = error "You need to implement this function."

