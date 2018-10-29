module Solutions8
(
) where

import Solutions1
import Solutions2
import Solutions3
import Solutions4
import Solutions5
import Solutions6
import Solutions7

data Tree a = Node a [Tree a] deriving (Eq,Show)

--problem 70B (impossible to be false in haskell)

--problem 70C
nnodes :: Tree a -> Int
nnodes (Node x []) = 1
nnodes (Node a children) = 1 + foldr ((+) . nnodes) 0 children




