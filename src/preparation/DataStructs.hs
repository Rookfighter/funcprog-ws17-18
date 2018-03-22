-- DataStructs.hs
--
--     Author: Fabian Meyer
-- Created On: 22 Mar 2018

module DataStructs where

class Insert f where
    insert :: (Ord a) => a -> f a -> f a
class Contains f where
    contains :: (Ord a) => a -> f a -> Bool

class Rank f where
    rank :: f a -> Int
class Root f where
    root :: f a -> a
class Merge f where
    merge :: (Ord a) => f a -> f a -> f a
class FindMin f where
    findMin :: (Ord a) => f a -> a
class DeleteMin f where
    deleteMin :: (Ord a) => f a -> f a

-- Binary Search Tree:
-- Elements in left Branch are less than root
-- Elements in right Branch are greater than root
data Tree a = Leaf | Branch a (Tree a) (Tree a)
    deriving (Show)

instance Insert Tree where
    insert x Leaf = Branch x Leaf Leaf
    insert x (Branch y t1 t2) = case compare x y of
                EQ -> Branch y t1 t2 -- every element is unique
                LT -> Branch y (insert x t1) t2
                GT -> Branch y t1 (insert x t2)

instance Contains Tree where
    contains x Leaf = False
    contains x (Branch y t1 t2) = case compare x y of
                EQ -> True
                LT -> contains x t1
                GT -> contains x t2

-- Heap:
-- The root is always the minimum element in the set.
-- Every node fullfills the heap condition: children are greater or equal to
-- node itself.
-- Leftist Heap:
-- Also fullfills leftist property: rank(left child) >= rank(right child)
-- Here rank of a node is length of its right spine.

data Heap a = EH | TH Int a (Heap a) (Heap a)
    deriving (Show)

instance Merge Heap where
    merge EH EH = EH
    merge EH h = h
    merge h EH = h
    merge h1@(TH _ a1 l1 r1) h2@(TH _ a2 l2 r2) =
        if a1 <= a2 then
            makeHeap a1 l1 (merge r1 h2)
        else
            makeHeap a2 l2 (merge r2 h1)

instance Rank Heap where
    rank EH = 0
    rank (TH r _ _ _) = r

makeHeap :: a -> Heap a -> Heap a -> Heap a
makeHeap a h1 h2 =
    if r1 >= r2 then
        TH (r2 + 1) a h1 h2
    else
        TH (r1 + 1) a h2 h1
    where r1 = rank h1
          r2 = rank h2

instance Insert Heap where
    insert a h = merge h (TH 1 a EH EH)
instance FindMin Heap where
    findMin (TH _ a _ _) = a
instance DeleteMin Heap where
    deleteMin (TH _ _ h1 h2) = merge h1 h2

-- Red Black Tree:
-- Over time binary search trees become unbalanced which degrades their O(log n)
-- insert and search time to O(n).
-- Red Black trees guarantee O(log n) search time. The following conditions
-- must hold:
-- 1 No red node has a red child.
-- 2 Every path from the root to a empty node contains the same number of black nodes.

data Color = Red | Black
    deriving (Show)

data RBTree a = RBE | RBT Color a (RBTree a) (RBTree a)

instance Contains RBTree where
    contains a RBE = False
    contains a (RBT _ b t1 t2) = case compare a b of
        EQ -> True
        LT -> contains a t1
        GT -> contains a t2

instance Insert RBTree where
    insert x s = RBT Black a t1 t2
        where (RBT _ a t1 t2) = insertT x s
              insertT a RBE = RBT Red a RBE RBE
              insertT a t@(RBT c b t1 t2) = case compare a b of
                  EQ -> t
                  LT -> balance c b (insertT a t1) t2
                  GT -> balance c b t1 (insertT a t2)

balance :: Color -> a -> RBTree a -> RBTree a -> RBTree a
balance Black z (RBT Red y (RBT Red x a b) c) d = RBT Red y (RBT Black x a b) (RBT Black z c d)
balance Black z (RBT Red x a (RBT Red y b c)) d = RBT Red y (RBT Black x a b) (RBT Black z c d)
balance Black x a (RBT Red z (RBT Red y b c) d) = RBT Red y (RBT Black x a b) (RBT Black z c d)
balance Black x a (RBT Red y b (RBT Red z c d)) = RBT Red y (RBT Black x a b) (RBT Black z c d)
balance c x a b = RBT c x a b

class QueueOps q where
    mtq :: q a -> Bool
    emptyq :: q a
    enq :: a -> q a -> q a
    tailq :: q a -> q a
    headq :: q a -> a


-- Queue:
-- In functional programming languages it is common to make the queue a pair of
-- lists. The first one hold the front elements then second holds rear elements
-- in reverse.
-- Invariant: Whenever front list is empty also rear list is empty.

data Queue a = Queue [a] [a]

instance QueueOps Queue where
    mtq (Queue f _) = null f
    emptyq = Queue [] []
    enq a (Queue f r) = checkQueue $ Queue f (a:r)
    tailq (Queue (_:fs) r) = checkQueue $ Queue fs r
    headq (Queue (f:_) _) = f

checkQueue (Queue [] r) = Queue (reverse r) []
checkQueue q = q
