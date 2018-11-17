module Solutions8
(nnodes,
 ipl
) where

import Solutions1
import Solutions2
import Solutions3
import Solutions4
import Solutions5
import Solutions6
import Solutions7

data Tree a = Node a [Tree a] deriving (Eq,Show)

tree5 = Node 'a' [
    Node 'f' [Node 'g' []],
    Node 'c' [],
    Node 'b' [Node 'd' [], Node 'e' []]
    ]

--problem 70C
nnodes :: Tree a -> Int
nnodes (Node x []) = 1
nnodes (Node a children) = 1 + foldr ((+) . nnodes) 0 children

--problem 71
ipl :: Tree a -> Int
ipl x = sum (iplHelper x)

iplHelper :: Tree a -> [Int]
iplHelper (Node _ []) = [0]
iplHelper (Node _ ys) = [0] ++ map (+1) (foldr (++) [] (map (iplHelper) ys))


