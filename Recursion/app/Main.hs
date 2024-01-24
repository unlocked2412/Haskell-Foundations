module Main where

import BinaryTrees
import Debug.Trace

bag :: Bag Int
bag = Bin' (Bin' (Bin' Tip' 2 Tip') 2 Tip') 4 (Bin' Tip' 5 Tip')

main :: IO ()
main = do
    print $ delete' 2 bag
