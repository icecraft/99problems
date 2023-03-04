
-- import System.Random


-- Problem 21 
-- Insert an element at a given position into a list.
{-
    (insert-at 'alfa '(a b c d) 2)
    =
        (A ALFA B C D)
-}
insertAt :: [a] -> [a] -> Int -> [a]
insertAt ix x pos = let (pre, post) = splitAt (pos-1) x 
                    in pre ++ ix ++ post 


-- Problem 22
-- Create a list containing all integers within a given range.
{-
    (range 4 9)
    = 
    (4 5 6 7 8 9)   
-}
range :: Int -> Int -> [Int]
range s e = take (e -s + 1) [s..]


{-
-- Problem 23 
-- Extract a given number of randomly selected elements from a list
{-
    (rnd-select '(a b c d e f g h) 3)
    =
        (E D A)
-}
rnd_select :: [a] -> Int -> IO [a]
rnd_select candidates count = do 
                g <- getStdGen 
                rs <- replicateM count $  (randomRs (0, length candidates-1) g)
                return $ map (candidates !! ) rs 

-- Problem 24 
-- Lotto: Draw N different random numbers from the set 1..M.
{-
    (diff-select 6 49)
    = 
        (23 1 17 33 21 37)
-}

diff_select :: Int -> Int -> [Int]
diff_select = TODO 



-- Problem 25 
-- Generate a random permutation of the elements of a list.
{-
    (rnd-permu '(a b c d e f))
    =
        (B A D C E F)
-}
rnd_permu :: [a] -> [a]
rnd_permu = TODO 

-}

-- Problem 26 
-- Generate the combinations of K distinct objects chosen from the N elements of a list
{-
    (combinations 3 '(a b c d e f))
    =
        ((A B C) (A B D) (A B E) ... )
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [] 
combinations c p@(y:ys) = if length p == c 
                            then [p]
                            else 
                                if length p > c 
                                    then 
                                    if c == 1 
                                        then [[x] | x <- p ]
                                        else (combinations c ys) ++ [  y: ll | ll <- combinations (c-1) ys ]
                                else []



-- Problem 27 
-- Group the elements of a set into disjoint subsets.
{-
    (group3 '(aldo beat carla david evi flip gary hugo ida))
    =
        ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
        ... )
-}

group27 :: [Int] -> [a] -> [[a]]
group27 =  TODO



{-
-- Problem 28 
-- Sorting a list of lists according to length of sublists
{-
    (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
    =
        ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
-}
lsort :: Ord a => [a] -> [a] 
lsort = TODO 


-}




