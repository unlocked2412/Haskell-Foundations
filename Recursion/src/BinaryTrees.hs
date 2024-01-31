{-# LANGUAGE DeriveFoldable #-}
module BinaryTrees where
import Data.Foldable (toList)
import Data.Function (on)

-- TREE exercises
{--

Let's start with a simple "unbalanced" binary search tree representing a set. 
This is a potentially very inefficient data structure, but many efficient data structures and their operations are based on this simple foundation.

-}
data Set a = Tip | Bin !(Set a) !a !(Set a) deriving (Show, Foldable)

instance Eq a => Eq (Set a) where
    (==) = (==) `on` toList

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

tree :: Set Int
tree = Bin (Bin (Bin Tip 0 Tip) 2 Tip) 4 (Bin Tip 5 Tip)

valid :: Ord a => Set a -> Bool
-- DONE: Gabriel, you should replace this with a real validity test.
-- It should return True if the Set obeys the invariant above, and False
-- otherwise.
valid Tip = True
valid (Bin l v r) = allTree (v >) l && allTree (v <) r && valid l && valid r

allTree :: (a -> Bool) -> Set a -> Bool
allTree _ Tip = True
allTree p (Bin l v r) = p v && allTree p l && allTree p r

minTree :: Ord a => Set a -> a
minTree Tip = error "No minimum value."
minTree (Bin l v r) = min v (minTree l)

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

-- When each element of s is less than every element of t,
-- join s t is the union of s and t.
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

splitAt n xs | n <= 0 = ([], xs)
splitAt _ [] = ([], [])
splitAt n (x : xs) = (x : front, rear)
  where (front, rear) = splitAt (n - 1) xs

Exercise 4c: Write a function

splitMaybe 4 [1,2,4,7] = ([1,2], True, [7])

FIXME
-}

data STriple a b c = STriple !a !b !c

-- Wrong output:
-- splitMember 1 (Bin Tip 2 (Bin Tip 3 Tip))
-- (Tip,False,Bin Tip 3 Tip)
splitMember :: Ord a => a -> Set a -> (Set a, Bool, Set a)
splitMember x s = 
    case splitMember' x s of
        STriple l found r -> (l, found, r)

-- LT | STriple l' found l'' <- splitMember' x l
--            -> STriple l' found (l'' `join` r)
splitMember' :: Ord a => a -> Set a -> STriple (Set a) Bool (Set a)
splitMember' _ Tip = STriple Tip False Tip
splitMember' x (Bin l v r) = 
    case compare x v of
        EQ -> STriple l True r
        LT | STriple l' found l'' <- splitMember' x l
            -> STriple l' found (Bin l'' v r)
        GT | case splitMember' x r of
            STriple r' found r'' -> STriple (Bin l v r') found r''

{-
1
Bin Tip 3 (Bin Tip 4 Tip)
[] /= [4]

1
Bin Tip 1 (Bin Tip 3 Tip)
[] /= [3]
-}

-- v in t: (l \/ {v} \/ r) /\ (tl \/ {v} \/ tr)
-- v not in t: (l \/ {v} \/ r) /\ (tl \/ tr)
-- David Feuer17:43
-- v not in t: (l /\ tl) \/ (l /\ tr) \/ ({v} /\ tl) \/ ({v} /\ tr) \/ (r /\ tl) \/ (r /\ tr)
-- David Feuer17:45
-- (l /\ tl) \/ (r /\ tr)
union :: Ord a => Set a -> Set a -> Set a
union s t = 
    case s of
        Tip -> t
        Bin l v r -> 
            case splitMember v t of
                (tl, _, tr) -> Bin (union l tl) v (union r tr)

intersection :: Ord a => Set a -> Set a -> Set a
intersection s t = 
    case s of
        Tip -> Tip
        Bin l v r -> 
            case splitMember v t of
                (tl, True, tr) -> Bin (intersection l tl) v (intersection r tr)
                (tl, False, tr) -> join (intersection l tl) (intersection r tr)
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
