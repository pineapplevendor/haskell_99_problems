module Solutions1
(getLast,
 getSecondToLast,
 getKth,
 countElements,
 reverseList,
 isPalindrome,
 flatten,
 compress,
 pack,
 encode
) where

-- problem 1
getLast :: [a] -> a
getLast [] = error "empty list"
getLast (x:[]) = x
getLast (x:xs) = getLast xs

--problem 2
getSecondToLast :: [a] -> a
getSecondToLast [] = error "empty list"
getSecondToLast (x:[]) = error "list of 1 item"
getSecondToLast (x:y:[]) = x
getSecondToLast (x:xs) = getSecondToLast(xs)

--problem 3
getKth :: (Integral b) => [a] -> b -> a
getKth [] k = error "empty list"
getKth x k
    | k == 0 = error "first element is index 1"
getKth (x:[]) k 
    | k > 1 = error "not enough elements"
    | k == 1 = x
getKth (x:xs) k
    | k > 1 = getKth xs (k-1)
    | k == 1 = x

--problem 4
countElements :: (Num b) => [a] -> b
countElements [] = 0
countElements (x:xs) = 1 + countElements xs

--problem 5
reverseList :: [a] -> [a]
reverseList xs = reverseListHelper xs []

reverseListHelper :: [a] -> [a] -> [a]
reverseListHelper [] y = y 
reverseListHelper (x:[]) y = x:y
reverseListHelper (x:xs) y = reverseListHelper xs (x:y)

--problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = ((reverseList xs) == xs)

--problem 7
--borrowed the data type from the solutions
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
flatten (List []) = []

--problem 8
compress :: Eq a => [a] -> [a]
compress (x:y:xs)
    | x == y = (compress (y:xs))
    | otherwise = x : (compress (y:xs))
compress xs = xs

--problem 9
pack :: Eq a => [a] -> [[a]]
pack x = reverseList (packHelper x [] [])

packHelper :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
packHelper [] cur all = cur:all
packHelper (x:xs) cur all
    | cur == [] = packHelper xs [x] all
    | x == (head cur) = packHelper xs (x:cur) all
    | otherwise = packHelper xs [x] (cur:all)

--problem 10
encode :: (Eq a, Integral b) => [a] -> [(b, a)]
encode x = map (listToRunLength) (pack x)

listToRunLength :: (Integral b) => [a] -> (b, a)
listToRunLength x = ((countElements x), (head x))

