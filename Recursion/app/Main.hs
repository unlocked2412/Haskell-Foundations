module Main where

import qualified MyLib (someFunc)
import BinaryTrees
import Debug.Trace

tree :: Set Int
tree = Bin (Bin (Bin Tip 0 Tip) 2 Tip) 4 (Bin Tip 5 Tip)

tree' = Bin Tip 3 (Bin Tip 4 Tip)
tree'' = Bin (Bin Tip 1 (Bin Tip 2 Tip)) 3 (Bin Tip 5 Tip)

bag :: Bag Int
bag = Bin' (Bin' (Bin' Tip' 2 Tip') 2 Tip') 4 (Bin' Tip' 5 Tip')

main :: IO ()
main = do
    print $ delete' 2 bag