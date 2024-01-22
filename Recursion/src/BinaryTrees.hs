{-# LANGUAGE DeriveFoldable #-}
module BinaryTrees where
import Data.Foldable (toList)

-- TREE exercises
{--

Let's start with a simple "unbalanced" binary search tree representing a set. 
This is a potentially very inefficient data structure, but many efficient data structures and their operations are based on this simple foundation.

-}
data Set a = Tip | Bin !(Set a) !a !(Set a) deriving (Show, Foldable)

{-

Invariant: in Bin l v r, each element of l is less than v, and each element of r is greater than v.

Exercise 1: I used the word "element" without defining it. In this case, it's the most "obvious" definition. 
Write a recursive definition (in English) of what it means for a value x to be an element of a Set s. 
Try to write the *simplest* definition you can while being precise.

x is not an element of (Tip :: Set a), and
x is an element of (Bin l v r :: Set a) iff
    x is equal to v, or x is an element of l, or x is an element of r

Exercise 2: Write a function

member :: Ord a => a -> Set a -> Bool

with the property that

member x s is True if x is a member of s and false otherwise. Use the ordering invariant to avoid exploring any parts of the tree you don't have to.

-}

valid :: Ord a => Set a -> Bool
-- FIXME: Gabriel, you should replace this with a real validity test.
-- It should return True if the Set obeys the invariant above, and False
-- otherwise.
valid s = True

member :: Ord a => a -> Set a -> Bool
member _ Tip = False
member x (Bin l v r) | x == v = True
                     | x < v  = member x l
                     | otherwise = member x r
{-

Exercise 3a: Write a function

insert :: Ord a => a -> Set a -> Set a

that inserts a value into a set, maintaining the order invariant. If the value is already an element of the set, the set should not be changed.

-}
insert :: Ord a => a -> Set a -> Set a
insert x Tip = Bin Tip x Tip
insert x t@(Bin l v r) | x == v = t
                       | x < v = Bin (insert x l) v r
                       | otherwise = Bin l v (insert x r)

{-

Exercise 3b: Write a function

insertMaybe :: Ord a => a -> Set a -> Maybe (Set a)

that inserts a value into a set, maintaining the order invariant. If the value is already an element of the set, returns Nothing.

-}
insertMaybe :: Ord a => a -> Set a -> Maybe (Set a)
insertMaybe x Tip = pure (Bin Tip x Tip)
insertMaybe x (Bin l v r) | x == v = Nothing 
                            | x < v = (insertMaybe x l) >>= \l' -> pure (Bin l' v r)
                            | otherwise = (insertMaybe x r) >>= \r' -> pure (Bin l v r')

{-

Exercise 4a: Write a function

delete  :: Ord a => a -> Set a -> Set a

that deletes an element from a set. If the element is not in the set, the set should be returned unchanged.

-}
delete :: Ord a => a -> Set a -> Set a
delete _ Tip = Tip
delete x (Bin l v r) | x < v = Bin (delete x l) v r
                     | x > v = Bin l v (delete x r)
                     | otherwise = join l r

minView :: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView (Bin l x r) = 
    case minView l of
        Nothing -> Just (x, r)
        Just (m, l') -> Just (m, Bin l' x r) -- m l' x r

maxView :: Set a -> Maybe (Set a, a)
maxView Tip = Nothing
maxView (Bin l x r) = 
    case maxView r of
        Nothing -> Just (l, x)
        Just (r', m) -> Just (Bin l x r', m) -- r x l m

join :: Set a -> Set a -> Set a
join s t = 
    case maxView s of
        Nothing -> t
        Just (s', m) -> Bin s' m t


{-

Exercise 4b: Write a function

deleteMaybe  :: Ord a => a -> Set a -> Maybe (Set a)

that deletes an value from a set if it is an element of the set and returns Nothing otherwise.
-}
deleteMaybe :: Ord a => a -> Set a -> Maybe (Set a)
deleteMaybe _ Tip = Nothing
deleteMaybe x (Bin l v r) 
    | x < v     = deleteMaybe x l >>= \l' -> pure (Bin l' x r)
    | x > v     = deleteMaybe x r >>= \r' -> pure (Bin l x r')
    | otherwise = pure (join l r)
                    
{-

Exercise 5: The same data structure can also be used to implement a "bag", sometimes called a "multiset"

data Bag a = Tip | Bin !(Bag a) !a !(Bag a)

The invariant is slightly different though: in Bin l v r, all the elements of l must be less than *or equal to* v, 
while all elements of r must be strictly greater than v. Write versions of:
- member, 
- insert (which inserts an additional copy of an element if it's already there), 
- delete (which only deletes one copy of an element), and 
- deleteMaybe (which also only deletes one copy).

-}

data Bag a = Tip' | Bin' !(Bag a) !a !(Bag a) deriving Show

member' :: Ord a => a -> Bag a -> Bool
member' _ Tip' = False
member' x (Bin' l v r) | x == v = True
                     | x < v  = member' x l
                     | otherwise = member' x r

insert' :: Ord a => a -> Bag a -> Bag a
insert' x Tip' = Bin' Tip' x Tip'
insert' x t@(Bin' l v r) | x == v = Bin' (insert' x l) v r
                         | x < v = Bin' (insert' x l) v r
                         | otherwise = Bin' l v (insert' x r)

maxView' :: Bag a -> Maybe (Bag a, a)
maxView' Tip' = Nothing
maxView' (Bin' l x r) = 
    case maxView' r of
        Nothing -> Just (l, x)
        Just (r', m) -> Just (Bin' l x r', m) -- r' x l m

join' :: Bag a -> Bag a -> Bag a
join' s t = 
    case maxView' s of
        Nothing -> t
        Just (s', m) -> Bin' s' m t

delete' :: Ord a => a -> Bag a -> Bag a
delete' _ Tip' = Tip'
delete' x (Bin' l v r) | x < v = Bin' (delete' x l) v r
                       | x > v = Bin' l v (delete' x r)
                       | otherwise = join' l r

deleteMaybe' :: Ord a => a -> Bag a -> Maybe (Bag a)
deleteMaybe' _ Tip' = Nothing
deleteMaybe' x (Bin' l v r) 
    | x < v     = deleteMaybe' x l >>= \l' -> pure (Bin' l' x r)
    | x > v     = deleteMaybe' x r >>= \r' -> pure (Bin' l x r')
    | otherwise = pure (join' l r)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
