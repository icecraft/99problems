
import Data.List 


-- Problem 11
-- Modified run-length encoding
{-
    (encode-modified '(aaaabccaadeeee))
-}

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified x = map conv $ group x 
                where 
                    conv y = case length y of 
                             1 -> Single (head y)
                             _ -> Multiple (length y) (head y)


-- Problem 12 
-- Decode a run-length encoded list
{-

    decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e'] 
    = 
        "aaaabccaadeeee"
-}

decodeModified :: Eq a => [ListItem a] -> [a] 
decodeModified = concatMap decode1 
            where 
                decode1 (Single x) = [x]
                decode1 (Multiple c x) = take c $ repeat x 



-- Problem 13 
-- Run-length encoding of a list (direct solution)
{-
    (encode-direct '(aaaabccaadeeee))
    =
        ((4 A) B (2 C) (2 A) D (4 E))
-}
encodeDirect1 :: Eq a => [a] -> [(Int, a)]
encodeDirect1 = foldr conv [] 
            where 
                conv x [] = [(1, x)]
                conv x ta@(y@(c, v):ys) = if x == v 
                                        then (c+1, v): ys
                                        else (1, x):ta 

                                    
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map decode1 . encodeDirect1 
            where 
                decode1 (1, x) = Single x 
                decode1 (n, x) = Multiple n x 


-- Problem 14 
-- Duplicate the elements of a list
{-
    (dupli '(a b c c d))
        (A A B B C C C C D D)
-}
dupC :: Int -> a -> [a]
dupC c = take c . repeat 

dupli :: [a] -> [a]
dupli = concatMap (dupC 2)


-- Problem 15 
-- Replicate the elements of a list a given number of times.
{-
    (repli '(a b c) 3)
    =
        (A A A B B B C C C)
-}
repli :: [a] -> Int -> [a] 
repli (x:xs) c = (dupC 3 x) ++ repli xs c 



-- Problem 16 
-- Drop every N'th element from a list.
{-

    (drop '(a b c d e f g h i k) 3)
    =
        (A B D E G H K)
-}
drop3 = map fst .  filter (\(x, _) -> (mod x 3) == 0) . zip [1..]



-- Problem 17 
-- Split a list into two parts; the length of the first part is given.
{-
     (split '(a b c d e f g h i k) 3)
       ( (A B C) (D E F G H I K))
-}
split17 :: [a] -> Int -> ([a], [a])
split17 = flip splitAt 



-- Problem 18 
-- Extract a slice from a list.
{-
    (slice '(a b c d e f g h i k) 3 7)
    =
        (C D E F G)
-}
slice :: [a] -> Int -> Int -> [a]
slice x s e = let (_, res) = splitAt s x
            in take (e - s) res 




-- Problem 19
-- Rotate a list N places to the left.
{-
(rotate '(a b c d e f g h) 3)
    (D E F G H A B C)
-}
rotate :: [a] -> Int -> [a]
rotate x c = let (p1, p2) = splitAt c x 
            in p2 ++ p1 


-- Problem 20 
-- Remove the K'th element from a list.
{-
    (remove-at '(a b c d) 2)
    =
        (A C D)
-}
removeK :: [a] -> Int -> [a]
removeK x c = let (p1, p2) = splitAt c x
                in (init p1) ++ p2 



