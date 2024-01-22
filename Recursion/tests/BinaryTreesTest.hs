{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified BinaryTrees as S
import BinaryTrees (Set (..))

import Test.QuickCheck
import Test.QuickCheck.Poly (OrdA)
import System.Exit

import Data.Foldable (toList)
import qualified Data.List as L

import ArbitrarySet ()

-- | A version of S.valid that produces a Property labeling the failure
-- as being caused by an invalid Set.
valid :: Ord a => Set a -> Property
valid = counterexample "Invalid Set" . S.valid

-- Test that the Arbitrary instance only generates valid Sets.
prop_arbitrary_valid :: Set OrdA -> Property
prop_arbitrary_valid s = valid s

-- A basic test for S.member.
prop_member :: OrdA -> Set OrdA -> Property
prop_member a s = (a `L.elem` toList s) === (a `S.member` s)

prop_delete :: OrdA -> Set OrdA -> Property
-- The .&&. operation combines any two Testable things to make a Property
-- that holds only if each of them does. The === operator is an informative
-- equality test. That is, if things should be equal but aren't, its failure
-- message will explain what was unexpectedly unequal.
prop_delete a s =
  valid s' .&&.
  toList s' === L.delete a (toList s)
  where
    s' = S.delete a s

-- Test that inserting an element does what we expect relative to the
-- understanding of Sets as representing strictly increasing lists.
prop_insert :: OrdA -> Set OrdA -> Property
-- FIXME: Gabriel, you should write this property. You should base
-- it on ideas from prop_delete above, and use the insertList function
-- below.
prop_insert a s = property True

-- Given a value and a *strictly increasing* list, add the value to the
-- list, producing a strictly increasing list containing it. For example:
--
-- insertList 3 [1,2,4] = [1,2,3,4]
-- insertList 3 [1,2,3,4] = [1,2,3,4]
-- insertList 3 [4,3] --- Invalid use.
-- insertList 1 [2,3] = [1,2,3]
insertList :: Ord a => a -> [a] -> [a]
-- The insert function from Data.List is almost what we want, but it
-- does not throw away duplicate elements.
insertList a l = removeAdjDuplicates (L.insert a l)

-- | Remove adjacent duplicate elements from a list.
removeAdjDuplicates :: Eq a => [a] -> [a]
-- We use the group function to gather together equal adjacent elements,
-- and then take only the first element of each group.
removeAdjDuplicates xs = [ e | (e : _) <- L.group xs]

-- FIXME: Gabriel, you should add property tests for the rest of the
-- Set API that verify it works properly relative to similar functions
-- on strictly ordered lists.


-- Interactions. The following tests check for the ways we expect certain
-- operations to interact.

-- Note: we don't *have* to produce an informative Property if we don't
-- want. We can just provide a Bool, and QuickCheck will deal with it
-- anyway
prop_member_insert :: OrdA -> Set OrdA -> Bool
prop_member_insert a s = a `S.member` (S.insert a s)

prop_not_member_deleted :: OrdA -> Set OrdA -> Bool
prop_not_member_deleted a s = not $ a `S.member` S.delete a s

prop_delete_preserves_others :: OrdA -> Set OrdA -> Property
-- conjoin takes a list of properties and produces a property
-- that requires all of them to hold.
--
-- ==> only requires a property to hold if a particular precondition
-- holds. In this case, we only check that an element is still in
-- the set if it is not the element we're deleting.
--
-- FIXME: There's nothing *wrong* with this test, but it's a bit "inefficient"
-- because it doesn't test anything about the deleted element. Can you
-- rewrite this test to make it do something useful in that case? Hint: you
-- will no longer use the ==> operator.
prop_delete_preserves_others to_delete s = conjoin
  [ a /= to_delete ==> counterexample ("Element " ++ show a ++ " was not preserved.")
                                      (a `S.member` S.delete to_delete s)
  | a <- toList s ]

-- FIXME: Gabriel, can you think of any other good tests
-- combining multiple operations?

return []

properties :: IO Bool
properties = $quickCheckAll

main :: IO ()
main = do
  passed <- properties
  exitWith $ if passed
    then ExitSuccess
    else ExitFailure 1
