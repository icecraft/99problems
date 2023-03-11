

data Tree a = Node a [Tree a]
        deriving (Eq, Show)


tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- Problem 70B 
-- Check whether a given term represents a multiway tree.
{-

-}

isTree :: Tree a -> Bool
isTree = True 



-- Problem 70C 
-- Count the nodes of a multiway tree.
nnodes :: Tree a -> Int 
nnodes = errors "todo"



-- Problem 70 
-- Tree construction from a node string.
{-

    stringToTree "afg^^c^bd^e^^^"
    =
    Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]\
-}
stringToTree :: String -> Tree a
stringToTree = errors "todo"



-- Problem 71 
-- Determine the internal path length of a tree.
{-
    ipl tree5 
    = 
        9
-}
ipl :: Tree a -> Int 
ipl = errors "todo"



-- Problem 72 
-- Construct the bottom-up order sequence of the tree nodes.
{-
    bottom_up tree5
    =
        "gfcdeba"
-}
bottom_up :: Tree a -> String 
bottom_up = errors "todo"



-- Problem 73 
-- Lisp-like tree representation.
{-
    display lisp tree5
    =
        "(a (f g) c (b d e))"
-}
display :: Tree a -> String 
display = errors "todo"



