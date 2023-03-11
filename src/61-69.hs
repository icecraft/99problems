

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)


-- Problem 61
-- Count the leaves of a binary tree
{-
    countLeaves tree4
    = 
        2
-}
countLeaves :: Tree a -> Int
countLeaves Empty = 0 
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + (countLeaves right)


-- Problem 61A
-- Collect the leaves of a binary tree in a list
{-
    leaves tree4
    = 
        [4, 2]
-}
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ left right) = leaves left ++ (leaves right)


-- Problem 62 
-- Collect the internal nodes of a binary tree in a list
{-
    internals tree4
    = 
        2
-}
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v a b) = [v] ++ (internals a) ++ (internals b)


-- Problem 62B
-- Collect the nodes at a given level in a list
{-
    atLeavel tree4 
    = 
        2
-}
atLeavel :: Tree a -> Int -> [a]
atLeavel Empty _ = []
atLeavel (Branch v _ _) 1 = [v]
atLeavel (Branch _ a b) n = (atLeavel a (n-1)) ++ (atLeavel b (n-1))


-- Problem 63
-- Construct a complete binary tree
{-
    completeBinaryTree 4
    = 
        Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
-}
completeBinaryTree :: Int -> Tree Char 
completeBinaryTree n = generate 1 
        where 
            generate m  | m > n = Empty 
                        | otherwise = Branch 'x' (generate $ 2*m) (generate $ 2*m + 1)

{-

{-
-- Problem 64 

-}



{-
-- Problem 65
-}



{-
-- Problem 66 
-}

-}



-- Problem 67A 
-- A string representation of binary trees
{-
    stringToTree "x(y,a(,b))" >>= print
    =
    Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
-}
findP :: String -> Int 
findP = findP1 0 0 
    where 
        findP1 _ _ "" = -1
        findP1 c pos (x:xs) = case x of 
                                ',' -> 
                                '(' -> findP1 (c+1) (pos+1) xs 
                                ')' -> findP1 (c-1) (pos+1) xs 


stringToTree :: String -> Tree a
stringToTree s = case length s of 
                0 -> Empty
                1 -> Branch (s !! 0) Empty Empty
                _ -> let subStr = init . tail $ tail s
                         pos = findP pos 
                        (left, right) = splitAt pos 
                    in Branch (s !! 0) (stringToTree left) (stringToTree righjt) 
                       
                



-- Problem 68 
-- Preorder and inorder sequences of binary trees. We consider binary trees with nodes that are identified by single lower-case letters, 
-- as in the example of problem P67.
{-
-}


{-
-- Problem 69
-- Dotstring representation of binary trees.
tree2ds :: Tree a -> String 
tree2ds = errors "TODO"
-}