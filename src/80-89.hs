

data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)
			  
data Adjacency a = Adj [(a, [a])]
		   deriving (Show, Eq)

data Friendly a = Edge [(a, a)]
		  deriving (Show, Eq)


-- Problem 80
-- Write predicates to convert between the different graph representations.


graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj = errors "todo"


adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph = errors "todo"


graphToFri :: (Eq a) => Graph a -> Friendly a
graphToFri = errors "todo"


friToGraph :: (Eq a) => Friendly a -> Graph a
friToGraph = errors "todo"


adjToFri :: (Eq a) => Adjacency a -> Friendly a
adjToFri = errors "todo"


friToAdj :: (Eq a) => Friendly a -> Adjacency a
friToAdj = errors "todo"



-- Problem 81
-- Path from one node to another one
{-
    paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    =
        [[1,2,3,4],[1,3,4]]
-}
paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths = errors "todo"



-- Problem 82 
-- Cycle from a given node
{-
    Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. 
    The predicate should return all cycles via backtracking.

    cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    =
        [[2,3,4,2]]

    cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    =
        []
-}
cycle :: Int -> [(Int, Int)] -> [[Int]]
cycle = errors "todo"




-- Problem 83
-- Construct all spanning trees
{-
    Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. 
-}



-- Problem 84 
-- Construct the minimal spanning tree
{-
    prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
    =
        [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
-}




-- Problem 85
--  Graph isomorphism
{-
    Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 -> N2 such that for any nodes X,Y of N1, 
            X and Y are adjacent if and only if f(X) and f(Y) are adjacent.

-}




-- Problem 86 
-- Node degree and graph coloration




-- Problem 87 
--  Depth-first order graph traversal (alternative solution)



-- Problem 88 
-- Connected components (alternative solution)





-- Problem 89
-- Bipartite graphs



