

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

element-at :: [a] -> init -> a 
element-at s idx = s !! (idx -1 )



-- Problem 4 
-- Find the number of elements of a list. 
{-
    myLength [123, 456, 789]
    myLength "Hello, world!"
-}

myLength :: [a] -> int 
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

isPalindrome :: [a] -> Bool 
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
flatten (List b) = TODO 


-- Problem 8 
-- Eliminate consecutive duplicates of list elements. 
{-
    (compress '(a a a a b c c a a d e e e e)) = (a b c a d e)
-}

compress81 :: Eq a => [a] -> [a] -> [a]
compress81 [] ys = ys 
compress81 x:xs [] = compress81 xs [x] 
compress81 x:xs zs = if (last zs) == x 
                        then compress81 xs zs 
                     else compress81 xs zs:x


compress :: [a] -> [a]
compress xs = compress81 xs []  -- 不知道有没有更好的解法 



-- Problem 9 
--  Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 
{-
    (pack '(a a a a b c c a a d e e e e)) =
        ((A A A A) (B) (C C) (A A) (D) (E E E E))
-}
data Pack91 a = Pack91None | Pack91S a | Pack91M a int 

pack91 :: Eq a => [a] -> Pack91 a -> [Pack91 a] -> [Pack91 a]
pack91 [] p@(Pack91S _) res = res ++ [p]
pack91 [] p@(Pack91M _ _) res = res ++ [p]
pack91 x:xs p res = case p of 
                        Pack91None -> pack91 xs (Pack91 x) res 
                        y@(Pack91S v) -> if v == x then 
                                            pack91 xs (Pack91M v 2) res 
                                          else pack91 xs (Pack91S x) (res ++ [y])
                        y@(Pack91M v count) -> if v == x then 
                                               pack91 xs (Pack91M v count+1) res 
                                            else pack91 xs (Pack91S x) (res ++ [y])
pack :: Eq a => [a] -> [[a]]
pack xs = render $ pack91 xs Pack91None []
        where 
            render = map . render2
            render2 (Pack91S x) = [x]
            render2 (Pack91M x c) =  take c $ repeat x 
-- too ugly have any good suggesions ？

