

import Data.List 


-- Problem 1 
-- Find the last element of a list 
{-
    myLast [1,2,3] = 3
    myLast ['a', 'b', 'c'] = 'c'
-}

myLast :: [a] -> a
myLast = last 


-- Problem 2 
--  Find the last but one element of a list
{-
    myButLast [1,2,3,4] = 3 
    myButLast ['a'..'z'] = 'y'
-}

myButLast :: [a] -> a
myButLast  =  last . init 


-- Problem 3 
-- Find the K'th element of a list. The first element in the list is number 1. 
{-
    element-at [1,2,4, 8, 16] 3 = 4
-}

elementAt :: [a] -> Int -> a 
elementAt s idx = s !! (idx -1 )



-- Problem 4 
-- Find the number of elements of a list. 
{-
    myLength [123, 456, 789]
    myLength "Hello, world!"
-}

myLength :: [a] -> Int 
myLength = length 
 

-- Problem 5 
-- Reverse a list 
{-
    myReverse [1,2,3,4] = [4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse = reverse 


-- Problem 6 
-- Find out whether a list is a palindrome
{-
    isPalindrome [1,2,3] = False
-}

isPalindrome :: Eq a => [a] -> Bool 
isPalindrome s = s == reverse s 



-- Problem 7 
-- Flatten a nested list structure. 
{-
    flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) = 
            [1,2,3,4,5]
-}


data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List b) = concatMap flatten b


-- Problem 8 
-- Eliminate consecutive duplicates of list elements. 
{-
    (compress '(a a a a b c c a a d e e e e)) = (a b c a d e)
-}

compress81 :: Eq a => [a] -> [a] -> [a]
compress81 [] ys = ys 
compress81 (x:xs) [] = compress81 xs [x] 
compress81 (x:xs) zs = if (last zs) == x 
                        then compress81 xs zs 
                     else compress81 xs (zs ++ [x])

compress :: Eq a => [a] -> [a]
compress xs = compress81 xs []  -- 不知道有没有更好的解法 



-- Problem 9 
--  Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 
{-
    (pack '(a a a a b c c a a d e e e e)) =
        ((A A A A) (B) (C C) (A A) (D) (E E E E))
-}

pack :: Eq a => [a] -> [[a]]
pack = group 

-- too ugly have any good suggesions ？


-- Problem 10 
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
--  Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 
{-

    (encode '(a a a a b c c a a d e e e e)) = 
        ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-}
encode y = map (\x -> ( length x, head x)) $ group ys

            

