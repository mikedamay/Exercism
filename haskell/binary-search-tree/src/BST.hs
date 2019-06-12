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

instance Functor (BST) where
    fmap f Empty = Empty
    fmap f t = Node (f (bstValue' t)) (fmap f (bstLeft' t)) (fmap f (bstRight' t))

instance Applicative (BST) where
    pure a = Node a Empty Empty
    f <*> t = Node ((bstValue' f) (bstValue' t)) (f <*> (bstLeft' t)) (f <*> (bstRight' t))



-- data BST a = BST a (Maybe (BST a)) (Maybe (BST a)) deriving (Eq, Show)
data BST a = Node {bstValue' :: a, bstLeft' :: BST a, bstRight' :: BST a } | Empty deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ Empty _) = Nothing
bstLeft (Node _ l _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ Empty) = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue tree = Just (bstValue' tree)

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList xs = foldr (\x tree -> (insert x tree)) Empty (reverse xs)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x tree
--     | x == bstValue' tree = Node x (bstLeft' tree) (bstRight' tree)
    | x > bstValue' tree = Node (bstValue' tree) (bstLeft' tree) (insert x (bstRight' tree))
    | x <= bstValue' tree = Node (bstValue' tree) (insert x (bstLeft' tree)) (bstRight' tree)

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList tree = (toList (bstLeft' tree)) ++ [(bstValue' tree)] ++ (toList (bstRight' tree))

