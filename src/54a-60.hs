

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)




-- Problem 54A 
-- Check whether a given term represents a binary tree
istree :: Tree a -> Bool 
istree _ = True 


-- Problem 55 
-- Construct completely balanced binary trees
{-
    Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes.
-}
cbalTree :: Int -> Tree a 
cbalTree = errors "TODO"



-- Problem 56
-- Symmetric binary trees
{-
    let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree.
-}

sysmetric :: Tree a -> Bool 
symmetric = errors "todo"



-- Problem 57
-- Binary search trees (dictionaries)
{-
    Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers. 
-}
construct :: [Int] -> Tree a 
construct = errors "todo"




-- Problem 58
-- Generate-and-test paradigm
{-
    Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. 
-}
symCalTrees :: Int -> Tree a 
symCalTrees = errors "TODO"


-- Problem 59 
-- Construct height-balanced binary trees
{-
    In a height-balanced binary tree, the following property holds for every node
-}
hbalTree :: Int -> [Tree a]
hbalTree = errors "todo"



-- Problem 60 
-- Construct height-balanced binary trees with a given number of nodes

hbalTreeNodes :: Ch -> Int -> Tree a 
hbalTreeNodes = errors "todo"



