

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


reverserT :: Tree a -> Tree a 
reverserT Empty = Empty 
reverserT (Branch v a b) = Branch v (reverserT b) (reverserT a) 

-- Problem 54A 
-- Check whether a given term represents a binary tree
istree :: Tree a -> Bool 
istree _ = True 


-- Problem 55 
-- Construct completely balanced binary trees
{-
    Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes.
-}
cbalTrees :: Int -> [Tree Char]
cbalTrees n  | n == 0 = [Empty] 
            | n == 1 = [Branch 'a' Empty Empty]
            | otherwise = let p1 = (div (n-1) 2)
                              p2 = n -1 - p1
                          in [Branch 'a' l r | l <- cbalTrees p1, r <- cbalTrees p2] ++ [Branch 'a' l r | l <- cbalTrees p2, r <- cbalTrees p1]
                    


-- Problem 56
-- Symmetric binary trees
{-
    let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree.
-}

symmetric :: Tree Char -> Bool 
symmetric Empty = True 
symmetric (Branch _  a b) = compareS a b 
            where 
                compareS Empty Empty = True 
                compareS Empty (Branch _ _ _) = False 
                compareS (Branch _ _ _) Empty = False 
                compareS (Branch _ la lb) (Branch _ rb ra) = and [compareS la ra, compareS lb rb]  



-- Problem 57
-- Binary search trees (dictionaries)
{-
    Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers. 

    construct [3, 2, 5, 7, 1]
    =
        Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-}
construct :: [Int] -> Tree Int
construct [] = Empty
construct (x:xs) = Branch x (construct $ filter (\y -> x >= y) xs) (construct $ filter (\y -> y > x) xs)




-- Problem 58
-- Generate-and-test paradigm
{-
    Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. 
-}

symCalTrees :: Int -> [Tree Char] 
symCalTrees n | mod n 2 == 0 = []
              | otherwise = [Branch 'x' t (reverserT t) | t <- cbalTrees $ div (n-1) 2]


-- Problem 59 
-- Construct height-balanced binary trees
{-
    In a height-balanced binary tree, the following property holds for every node
-}
hTree :: Int -> [Tree Char]
hTree 0 = [Empty]
hTree 1 = [Branch 'x' Empty Empty]
hTree 2 = [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty),
            Branch 'x' Empty (Branch 'x' Empty Empty),
            Branch 'x' (Branch 'x' Empty Empty) Empty] 

hbalTree :: Int -> [Tree Char]
hbalTree n = [ Branch 'x' l r | l <- hTree (n-1), r <- hTree (n-1) ]
     


-- Problem 60 
-- Construct height-balanced binary trees with a given number of nodes

countTreeNode :: Tree Char -> Int 
countTreeNode Empty = 0
countTreeNode (Branch _ a b) = 1 + (countTreeNode a) + (countTreeNode b)


hbalTreeNodes :: Int -> [Tree Char] 
hbalTreeNodes n = filter (\x -> countTreeNode x == n) $ concatMap hbalTree [lower .. upper ]
            where 
                lower = ceiling $ logBase 2 $ fromIntegral(n+1)
                upper = div (n+1) 2

