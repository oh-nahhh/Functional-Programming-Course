{-
  Functional Programming 1
  Assignment 4
  Niklas Bergqvist
-}

 module Lab4(dot,connectedComponent) where

-- import your Graph module (not inline with the instructions)
import Graph

{- 3 -}

{-
  dot :: Graph String -> String
  Given a graph whose vertices are strings, dot computes a
  string representation of the graph in the DOT graph description language.
  PRE: Must be String.
  RETURNS: String.
-}

dot :: Graph String -> String
dot graph = "{\n" ++ concatMap (prGraph graph) (vertices graph)  ++ "}"

{-
  prGraph :: Graph String -> String -> String
  Helper function to dot.
  PRE: Must be String.
  RETURNS: String,
-}

prGraph :: Graph String -> String -> String
prGraph graph points = points ++ ";" ++ "\n" ++ (concatMap (sub points) (neighbors graph points))
    where  sub points name = points ++ " -- " ++ name ++ ";\n"

{- 4 -}
{-
  connectedComponent :: Eq a => Graph a -> a -> Graph a
  Find all the connected components of g that contains vertex vs.
  PRE: Vertices must be in graph.
  POST: The graph is unchanged.
  RETURNS: Graph a
-}

connectedComponent :: Eq a => Graph a -> a -> Graph a
connectedComponent g vs = foldl addEdge (foldl (\list x -> addVertex list x) (addVertex empty vs) points) (clear [(x,y) | x <- (vertices g), y <- (neighbors g x), x `elem` dfs g vs])
      where
          points = concatMap (neighbors g) (dfs g vs)

{-
  dfs :: Eq a => Graph a -> a -> [a]
  Generates a list of paths using the depth-first algorithm.
  PRE: Vertices already in the graph.
  RETURNS: List of vertices.
  VARIANT: List of nodes without neighbors in a graph.
-}

dfs :: Eq a => Graph a -> a -> [a]
dfs graph src = depth graph [src]
    where depth (Graph [] _) _ = []
          depth _ [] = []
          depth (Graph points edges) (top:pile)
              | [x | x<-points, x==top] == [] = depth (Graph points edges) pile
              | otherwise = top : depth (Graph (filter (\x -> x/=top) points) edges) ([y|(x,y)<-edges, x==top] ++ [x|(x,y)<-edges, y==top] ++ pile)

{-
  Clear ::Eq a => [(a,a)] -> [(a,a)]
  Removes doubles from the edges.
  RETURNS: list of tuples.
  VARIANT: list [(a,a)].
-}

clear ::Eq a => [(a,a)] -> [(a,a)]
clear [] = []
clear ((x,y):xs) | (y,x) `elem` xs = clear xs
                 | otherwise = (x,y) : clear xs
