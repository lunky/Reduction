module Reduction
    ( dirReduce
    ) where
import Reduction.Direction

timesvr :: Direction -> [Direction] -> [Direction]
timesvr x [] = [x]
timesvr x xs
  | x == (opposite (head xs)) = tail xs
  | otherwise = x:xs

dirReduce :: [Direction] -> [Direction]
dirReduce orig = foldr timesvr [] orig

opposite :: Direction -> Direction 
opposite North = South
opposite South = North
opposite East = West
opposite West = East 
