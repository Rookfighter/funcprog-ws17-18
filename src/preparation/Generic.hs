-- Generic.hs
--
--     Author: Fabian Meyer
-- Created On: 22 Mar 2018

module Generic where

type Name = String
type Arity = Int
data Unit = Unit
data Plus a b = Inl a | Inr b
data Pair a b = Pair {outl :: a, outr :: b}
data Iso a b = Iso { fromData :: b -> a, toData :: a -> b }

-- Class generic is a interface for generic functions!
class Generic f where
    unit :: f Unit
    plus :: f a -> f b -> f (Plus a b)
    pair :: f a -> f b -> f (Pair a b)
    constr :: Name -> Arity -> f a -> f a
    constr _ _ = id
    char :: f Char
    int :: f Int
    view :: Iso a b -> f a -> f b

-- Interface to implement a generic representation of a type.
class Rep a where
    rep :: Generic f => f a

-- some predefined instances
instance Rep Unit where
    rep = unit

instance (Rep a, Rep b) => Rep (Plus a b) where
    rep = plus rep rep

instance (Rep a, Rep b) => Rep (Pair a b) where
    rep = pair rep rep

instance Rep Char where
    rep = char

instance Rep Int where
    rep = int

-- representation for types with type constructors
class FRep a where
    frep :: Generic f => f b -> f (a b)

-- elementary representation of booleans.
type BoolF = Pair Unit Unit
isoBool = Iso fromBool toBool
fromBool True = Inl Unit
fromBool False = Inr Unit
toBool (Inl Unit) = True
toBool (Inr Unit) = False

instance Rep Bool where
    rep = view isoBool (plus unit unit)

newtype Count a = Count {count' :: a -> Int}

instance Generic Count where
    unit = Count (\_ -> 0)
    plus fa fb = Count (\x -> case x of Inl a -> count' fa a
                                        Inr b -> count' fb b)
    pair fa fb = Count (\(Pair a b) -> (count' fa a) + (count' fb b))
    char = Count (\_ -> 0)
    int = Count (\_ -> 0)
    view iso fa = Count (\b -> count' fa (fromData iso b))

count :: FRep a => a b -> Int
count a = count' (frep (Count (\_ -> 1))) a

-- Implement representation of a List with elementary data descriptors.
-- Also implement isomorphism to transform List to its elementary representation
-- and back.

type ListF a = Plus Unit (Pair a [a])

isoList :: Iso (ListF a) [a]
isoList = Iso fromList toList

fromList :: [a] -> (ListF a)
fromList [] = Inl Unit
fromList (a:as) = Inr (Pair a as)

toList :: (ListF a) -> [a]
toList (Inl Unit) = []
toList (Inr (Pair a as)) = a:as

rList :: Generic f => f a -> f [a]
rList fa = view isoList (plus unit (pair fa (rList fa)))

instance Rep a => Rep [a] where
    rep = rList rep

instance FRep [] where
    frep = rList

-- Tree implementation
data Tree a = Leaf | Branch a (Tree a) (Tree a)
type TreeF a = Plus Unit (Pair a (Pair (Tree a) (Tree a)))

isoTree :: Iso (TreeF a) (Tree a)
isoTree = Iso fromTree toTree
fromTree Leaf = Inl Unit
fromTree (Branch a t1 t2) = Inr (Pair a (Pair t1 t2))
toTree (Inl Unit) = Leaf
toTree (Inr (Pair a (Pair t1 t2))) = Branch a t1 t2

rTree :: Generic f => f a -> f (Tree a)
rTree fa = view isoTree (plus unit (pair fa (pair (rTree fa) (rTree fa))))

instance Rep a => Rep (Tree a) where
    rep = rTree rep

instance FRep Tree where
    frep = rTree
