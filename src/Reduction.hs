module Reduction
    ( dirReduce
    ) where
import Reduction.Direction

timesaver :: Direction -> [Direction] -> [Direction]
timesaver x [] = [x]
timesaver x xs
  | x == (opposite (head xs)) = tail xs
  | otherwise = x:xs

dirReduce :: [Direction] -> [Direction]
dirReduce orig = foldr timesaver [] orig

opposite :: Direction -> Direction 
opposite North = South
opposite South = North
opposite East = West
opposite West = East 
