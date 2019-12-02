module Functions 
( Tree,
  LL,
  qs,
  singleton,
  singletonList,
  treeInsert,
  listInsert
) where

---simple quicksort
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = 
    qs (filter (<=x) xs)  ++ [x] ++ (filter (>x) xs)

--- Tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton:: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert::(Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

--- Linked List
data LL a = EmptyLL | Nodule a (LL a) deriving (Show,Read, Eq)

singletonList:: a -> LL a
singletonList x = Nodule x (EmptyLL)

listInsert::a -> LL a -> LL a
listInsert x EmptyLL = singletonList x
listInsert x (Nodule ele lst) = Nodule x (Nodule ele lst)  

