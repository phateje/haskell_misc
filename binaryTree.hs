data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = singleton x
insert x (Node a left right)
    | x == a    = Node x left right -- this tree won't hold duplicates
    | x < a     = Node a (insert x left) right
    | x > a     = Node a left (insert x right)

isElem :: (Ord a) => a -> Tree a -> Bool
isElem x Empty = False
isElem x (Node a left right)
    | x == a    = True
    | x < a     = isElem x left
    | otherwise = isElem x right