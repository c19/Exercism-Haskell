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

data LinkedList a = Empty | Node a (LinkedList a) deriving (Eq, Show)

instance Foldable LinkedList where
  foldr f v Empty = v
  foldr f v (Node a list) = foldr f (f a v) list

datum :: LinkedList a -> a
datum (Node a _) = a

fromList :: [a] -> LinkedList a
fromList = foldr new Empty

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next (Node x linkedList) = linkedList

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldr new Empty

toList :: LinkedList a -> [a]
toList = foldr (\a b -> b ++ [a]) []
