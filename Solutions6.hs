module Solutions6
(cbalTree,
 isSymmetric,
 construct,
 symCbalTree
) where

import Solutions1
import Solutions2
import Solutions3
import Solutions4
import Solutions5

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

--problem 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree n = concat [createTrees 'x' (cbalTree a) (cbalTree b) | 
                a <- [0..n-1], b <- [0..n-1],
                (abs $ b-a) <= 1, a + b == n-1]

createTrees :: a -> [Tree a] -> [Tree a] -> [Tree a]
createTrees a ls rs = [Branch a l r | l <- ls, r <- rs]

--problem 56
isSymmetric :: Tree a -> Bool
isSymmetric Empty = True
isSymmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty (Branch _ _ _) = False
mirror (Branch _ _ _) Empty = False
mirror Empty Empty = True
mirror (Branch _ ll lr) (Branch _ rl rr) = mirror ll rr && mirror lr rl

--problem 57
construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct (x:xs) = add x (construct xs)

add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = leaf x
add x (Branch root left right)
    | x > root = Branch root left (add x right)
    | otherwise = Branch root (add x left) right

--problem 58
symCbalTree :: Int -> [Tree Char]
symCbalTree n = filter isSymmetric (cbalTree n)

--problem 59 && 60, nah

--for later if needed
depth :: Tree t -> Int
depth Empty = 0
depth (Branch x c1 c2) = 1 + max (depth c1) (depth c2)

