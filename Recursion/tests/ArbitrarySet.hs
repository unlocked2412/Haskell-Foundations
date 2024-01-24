{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitrarySet () where
import Test.QuickCheck
import Test.QuickCheck.Poly
import BinaryTrees (Set (..))
import qualified Data.Set as CS
import Data.Foldable (toList)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized $ \s -> do
      -- How many elements we will try to get. Depending on how things go, we may
      -- get fewer.  This could happen because there just aren't enough elements
      -- of the type, or because we can't generate enough of them with a
      -- "reasonable" number of attempts.
      k <- choose (0, s)

      -- We generate up to 2k + 4 elements, attempting to get k distinct ones.
      -- There's nothing special about 2k + 4; it's just an effort to
      -- ensure we get a decent chance of obtaining as many elements
      -- as we're hoping for even at small sizes and using types with relatively
      -- few elements.
      elts <- generateElements k (2 * k + 4)

      -- Arrange the elements into a tree of arbitrary shape.
      distribute (CS.size elts) (toList elts)

    shrink Tip = []
    shrink (Bin l v r) =
      -- shrink Bin to Tip
      [Tip] ++
      -- shrink to subterms
      [l, r] ++
      -- recursively shrink subterms
      [Bin l' v r' | (l', r') <- shrink (l, r)]

-- | @generateElements t m@ tries to generate @t@ distinct elements,
-- invoking the generator at most @m@ times. Yes, we could just generate
-- elements, sort them, and remove duplicates, but doing it this way
-- should give us a size closer to what is requested.
generateElements :: (Ord a, Arbitrary a) => Int -> Int -> Gen (CS.Set a)
generateElements = go CS.empty
  where
    go !s !_ 0 = pure s
    go s t m
      | CS.size s == t = pure s
      | otherwise = arbitrary >>= \e -> go (CS.insert e s) t (m - 1)

-- Given a list of elements and its exact size, generate a
-- Set of arbitrary shape with exactly those elements.
distribute :: Int -> [a] -> Gen (Set a)
distribute n _
  | n < 0 = error "distribute: negative size"
distribute n xs = distribute' n xs >>= \(s, xs') ->
  if null xs'
  then pure s
  else error "distribute: size mismatch"

-- distribute' n xs arranges the first n elements of xs into
-- a tree of arbitrary shape, returning the tree and the unused
-- elements. n must be a positive number no greater than the number
-- of elements in the list.
distribute' :: Int -> [a] -> Gen (Set a, [a])
distribute' 0 xs = pure (Tip, xs)
-- The special case for 1 isn't strictly necessary, but I believe it's
-- faster to have it than not.
distribute' 1 (x : xs) = pure (Bin Tip x Tip, xs)
distribute' n xs = do
  nl <- choose (0, n - 1)
  (!l, xs') <- distribute' nl xs
  case xs' of
    [] -> error "distribute: size mismatch"
    !v : xs'' -> do
      (!r, xs''') <- distribute' (n - nl - 1) xs''
      pure (Bin l v r, xs''')
