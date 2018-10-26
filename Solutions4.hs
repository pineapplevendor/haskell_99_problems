module Solutions4
(isPrime,
 getBigFactor,
 coprime,
 totientPhi,
 isPrimeFactor,
 primeFactorsMult,
 phi,
 primesR,
 goldbach,
 goldbachList,
 goldbachListFilter,
) where

import Solutions1
import Solutions2
import Solutions3

--problem 31
isPrime :: Int -> Bool
isPrime n = length (filter (flip isFactor n) [2..squareRoot]) == 0
    where squareRoot = floor $ sqrt (fromIntegral n :: Double)

isFactor :: Int -> Int -> Bool
isFactor x y = y `mod` x == 0

--problem 32 (woulda called it gcd but that's taken by Prelude)
getBigFactor :: Int -> Int -> Int
getBigFactor x y
    | isFactor (min x y) (max x y) = min x y
    | otherwise = getBigFactor (min x y) r
    where r = (max x y) `mod` (min x y)

--problem 33
coprime :: Int -> Int -> Bool
coprime x y = getBigFactor x y == 1

--problem 34
totientPhi :: Int -> Int
totientPhi 1 = 1
totientPhi x = length $ filter (coprime x) [1..x-1]

--problem 35
isPrimeFactor :: Int -> Int -> Bool
isPrimeFactor x y = isFactor x y && isPrime x

primeFactors :: Int -> [Int]
primeFactors x = filter (flip isPrimeFactor x) [1..x-1]

--problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult x = map flipTuple (encode $ getCompleteFactorization x)

getCompleteFactorization :: Int -> [Int]
getCompleteFactorization x
    | isPrime x = [x]
    | otherwise = factor:getCompleteFactorization (x `div` factor)
    where factor = head $ filter (flip isFactor x) [2..x-1]

flipTuple :: (a,b) -> (b,a)
flipTuple (x,y) = (y,x)

--problem 37
phi :: [(Int, Int)] -> Int
phi [] = 1::Int
phi ((p,m):xs) = ((p-1)*p) ^ (m-1)

--problem 38 (not required)

--problem 39
primesR :: Int -> Int -> [Int]
primesR x y = filter isPrime [x..y]

--problem 40
goldbach :: Int -> (Int,Int)
goldbach n  = head [(x,y) | x <- primesInRange, y <- primesInRange, x + y == n]
    where primesInRange = primesR 2 (n-1)

--problem 41
--problem 41 a
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n m = map goldbach $ filter even [max 4 n..m]

--problem 41 b

goldbachListFilter :: Int -> Int -> Int -> [(Int,Int)]
goldbachListFilter x y z = filter ((>z).min') (goldbachList x y)

min' :: (Int,Int) -> Int
min' (x,y) = min x y


