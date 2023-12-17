{-
I'm still looking for good online resources. I have heard very good
things about this book and its exercises, but I don't own it so I
can't suggest anything specific.

https://lorepub.com/product/haskellbook

There are some exercises here: https://hackmd.io/@alexhkurz/H1jUka4Gv
I don't know what the less_equal function it describes is supposed to
do, so I would probably skip that one. The bottom-up merge sort
exercise it gives is very nice, so I suggest you try it.

Write a recursive function

-}
reverseSlowly :: [a] -> [a]
reverseSlowly [] = []
reverseSlowly (a : as) = reverseSlowly as ++ [a]
{-

that reverses a list. It should look like this:

reverseSlowly [] = ?
reverseSlowly (a : as) = ?

Can you understand what makes it slow?

******************** ANSWER ********************
I am not completely sure, but looking at (++) definition:

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

we see recursion is done on the first parameter so I think (++) can't start working until
`reverseSlowly` is fully evaluated.
************************************************

Write a recursive function

-}
reverseOnto :: [a] -> [a] -> [a]
reverseOnto [] ys = ys
reverseOnto (x:xs) ys = reverseOnto xs (x:ys)


{-

that reverses one list onto the front of another one:

reverseOnto [1,2,3,4] [10,20,30,40,50]
= [4,3,2,1,10,20,30,40,50]

This *generalizes* the reverse function, since

reverse xs = reverseOnto xs []

Define

reverseQuickly xs = reverseOnto xs []

Can you understand why reverseQuickly is faster than reverseSlowly?

******************** ANSWER ********************
It's faster because the function is fusing append and reverse. In the recursive step,
doesn't use append but instead uses cons in each recursive call.
************************************************

Consider this tree type (a binary leaf tree):

-}
data LT a = Leaf a | Branch (LT a) (LT a) deriving Show
{-

Write a recursive function

-}
sumLT :: Num a => LT a -> a
sumLT (Leaf x) = x
sumLT (Branch l r) = sumLT l + sumLT r

{-

that adds up all the values in a tree.

Write the simplest recursive function you can think of for

-}
ltToList :: LT a -> [a]
ltToList (Leaf x) = [x]
ltToList (Branch l r) = ltToList l ++ ltToList r
{-

that lists the elements of an LT from left to right. The
implementation should look like this:

ltToList (Leaf a) = ?
ltToList (Branch l r) = ?

After you have done this, think about performance. What is the worst
case? 
******************** ANSWER ********************
The worst case occurs when the left branch has more nodes than the right one.
************************************************


Now write a slightly different recursive function

-}
ltToListOnto :: LT a -> [a] -> [a]
ltToListOnto (Leaf x)     ys = x:ys 
ltToListOnto (Branch l r) ys = ltToListOnto l (ltToListOnto r ys)
{-

This should convert a tree to a list and prepend it to the list given
as an argument:

ltToListOnto (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)) [10, 20]
= [1,2,3,10,20]

Can you use ltToListOnto to write a more efficient version of ltToList?
-}

ltToList' :: LT a -> [a]
ltToList' tree = ltToListOnto tree []

-- Extra exercises

main :: IO ()
main = print $ ltToListOnto (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)) [10,20]