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
data BST a = Node a (BST a) (BST a) | Empty deriving (Eq, Show)

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
bstValue (Node a _ _) = Just a

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList xs = foldr (\x tree -> (insert x tree)) Empty (reverse xs)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node a l r)
    | x > a = Node a l (insert x r)
    | x <= a = Node a (insert x l) r
insert _ (Node _ _ _) = error "why does it want this sure insert x (Node a l r) covers it."

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node a l r) = (toList l) ++ [a] ++ (toList r)

