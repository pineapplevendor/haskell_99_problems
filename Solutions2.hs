module Solutions2
(encodeModified,
 decodeModified,
 encodeDirect,
 dupli,
 repli,
 dropEvery
) where

import Solutions1

--problem 11
--borrowed the datatype syntax from the solutions
data ListItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs = map (toModifiedCode) (encode xs)

toModifiedCode :: (Eq a) => (Int,a) -> ListItem a
toModifiedCode (1,x) = Single x
toModifiedCode (n,x) = Multiple n x

--problem 12
decodeModified :: [ListItem a] -> [a]
decodeModified xs = reverse (decodeModifiedHelper xs [])

decodeModifiedHelper :: [ListItem a] -> [a] -> [a]
decodeModifiedHelper (x:[]) ys = (runLengthToList x) ++ ys
decodeModifiedHelper (x:xs) ys = decodeModifiedHelper xs ((runLengthToList x) ++ ys)

runLengthToList :: (ListItem a) -> [a]
runLengthToList (Single a) = [a]
runLengthToList (Multiple n x) = replicate n x

--problem 13
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect xs = reverse (encodeDirectHelper xs 1 [])

encodeDirectHelper :: (Eq a) => [a] -> Int -> [ListItem a] -> [ListItem a]
encodeDirectHelper (x:y:xs) seqLen matched
    | x == y = encodeDirectHelper (y:xs) (seqLen + 1) matched
    | otherwise = encodeDirectHelper (y:xs) 1 ((countToListItem x seqLen):matched)
encodeDirectHelper (x:[]) seqLen matched = ((countToListItem x seqLen):matched)

countToListItem :: a -> Int -> (ListItem a)
countToListItem x n
    | n == 1 = Single x
    | otherwise = Multiple n x

--problem 14
dupli :: [a] -> [a]
dupli xs = dupliHelper xs []

dupliHelper :: [a] -> [a] -> [a]
dupliHelper (x:xs) ys =  x:x:(dupliHelper xs ys)
dupliHelper [] ys = ys

--problem 15
repli :: [a] -> Int -> [a]
repli [] y = []
repli (x:xs) y = (replicate y x) ++ (repli xs y)

--problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryHelper xs n n 

dropEveryHelper :: [a] -> Int -> Int -> [a]
dropEveryHelper [] n m  = []
dropEveryHelper (x:xs) n m 
    | n == 1 = (dropEveryHelper xs m m)
    | otherwise = x:(dropEveryHelper xs (n-1) m)

--problem 17
split :: [a] -> Int -> [[a]]
split xs n = [getFirstN xs n, reverse $ getFirstN (reverse xs) m]
    where m = countElements xs - n

getFirstN :: [a] -> Int -> [a]
getFirstN (x:xs) n
    | n == 0 = []
    | otherwise = x:(getFirstN xs (n-1))

