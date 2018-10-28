module Solutions7
(countLeaves,
 leaves,
 internals,
 atLevel,
 completeBinaryTree
) where

import Solutions1
import Solutions2
import Solutions3
import Solutions4
import Solutions5
import Solutions6

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

--problem 61
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

--problem 61a
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

--problem 62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = [x] ++ internals l ++ internals r

--problem 62b
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n = (atLevel l (n-1)) ++ (atLevel r (n-1))

--problem 63 
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = addComplete 'x' (completeBinaryTree (n-1))

addComplete :: (Ord a) => a -> Tree a -> Tree a
addComplete x Empty = leaf x
addComplete x (Branch root l r)
    | getMinDepth r < getMinDepth l = Branch root l (addComplete x r)
    | otherwise = Branch root (addComplete x l) r

getMinDepth :: Tree a -> Int
getMinDepth Empty = 0
getMinDepth (Branch _ l r) = 1 + min (getMinDepth l) (getMinDepth r)




