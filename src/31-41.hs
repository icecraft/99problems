

import Data.List 


-- Problem 31 
-- Check whether a given number is prime 
{-
    isPrime 7 
    = 
        true 
-}
_prime :: Int -> Int -> [Int] -> [Int]
_Prime 1 _ _  = []
_prime 2 _ _  = [2]
_prime n inc xs = if inc > n 
                    then xs 
                  else  
                    case all (\x -> mod inc x > 0) xs of 
                        True -> _prime n (inc+2) (inc:xs) 
                        False -> _prime n (inc+2) xs 


primeR :: Int -> [Int] 
primeR n = _prime n 3 [2]


isPrime :: Int -> Bool
isPrime n 
        | 0 >= n = False 
        | otherwise = let ll = _prime n 3 [2] 
                    in (head ll) == n


-- Problem 32 
-- Find the GCD number 
{-
    gcd (36 63)
    = 
        9
-}
myGCD :: Int -> Int -> Int 
myGCD a b = let (x, y) = (max a b, min a b)
            in  if y > 0
                    then myGCD y $ mod x y
                    else x 


-- Problem 33 
-- coprime check 
{-
    coprime 35 64 
    = 
        true 
-}
coprime :: Int -> Int -> Bool
coprime x y = 1 == myGCD x y 


-- Problem 34 
-- Euler totient function phi 
{-
    Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

    Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1. 

    totientPhi 10 
    = 
        4
-}
totient :: Int -> Int 
totient n = foldr (\x r -> if coprime x n then r +1 else r ) 0 [1..n-1]



-- Problem 35 
-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order. 
{-
    primeFactors 315 
    = 
        ( 3 3 5 7)
-}
dofactors :: Int -> [Int] -> [Int] -> [Int]
dofactors _ [] res = res
dofactors n xs res = let cs = filter (\x -> mod n x == 0) xs
                    in dofactors (div n $ product cs) (filter (\x -> n >=x ) xs) (cs ++ res)
                

primeFactors :: Int -> [Int]
primeFactors n =  dofactors n  (_prime n 3 [2] ) []
                  


-- Problem 36 
-- Determine the prime factors of a given positive integer. 
{-
    primeFM 315 
    = 
        ((3 2) (5 1) (7 1))
-}
primeFM :: Int -> [(Int, Int)]
primeFM = map convert . groupBy (\a b -> a == b) . sort . primeFactors 
        where 
            convert xs = (head xs, length xs)



-- Problem 37 
-- Calculate Euler's totient function phi(m) (improved). 
{-
    See problem 34 for the definition of Euler's totient function. 
    If the list of the prime factors of a number m is known in the form of problem 36 
    then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) 
    be the list of prime factors (and their multiplicities) of a given number m. 
    Then phi(m) can be calculated with the following formula: 

-}
-- TODO


-- Problem 38 
--  Compare the two methods of calculating Euler's totient function. 

-- TODO

-- Problem 39
--  A list of prime numbers. 
{-
    Given a range of integers by its lower and upper limit, 
    construct a list of all prime numbers in that range. 

    primesR 10 20
    =
        [11,13,17,19]

-}

primesR :: Int -> Int -> [Int]
primesR lower upper = filter (\x -> x > lower && upper > x ) $ _prime upper 3 [2]




-- Problem 40 
-- Goldbach's conjecture. 
{-
    goldbach 28 
    = 
        (5  23)

-}
goldbach :: Int -> (Int, Int)
goldbach n = convert . head $  filter (\x -> isPrime $ n - x ) primes
    where 
        primes = _prime n 3 [2]
        convert x = (x, n -x)



-- Problem 41
-- Given a range of integers by its lower and upper limit, 
-- print a list of all even numbers and their Goldbach composition. 
{-
    (goldbach-list 9 20)
    = 
        10 = 3 + 7
        12 = 5 + 7
        14 = 3 + 11
        16 = 3 + 13
        18 = 5 + 13
        20 = 3 + 17
-}
goldbachsR :: Int -> Int -> [(Int, Int)]
goldbachsR lower upper = map goldbach $ filter (\x -> mod x 2 == 0) [lower .. upper]




