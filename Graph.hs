{-
  Functional Programming 1
  Assignment 4
  Niklas Bergqvist
-}

module Graph(Graph(..),empty,addVertex,addEdge,vertices,neighbors) where

import Data.List

data Graph a = Graph { vs :: [a], edges :: [(a,a)] } deriving Eq

{- 1.1 -}
{-
  empty :: Graph a
  Obtain an empty graph.
  RETURNS: An empty graph.
-}

empty :: Graph a
empty = Graph [] []

{- 1.2 -}
{-
  addVertex :: Eq a => Graph a -> a -> Graph a
  Adding a new vertex to the graph.
  PRE: Vertex is not already in the graph.
  POST: A vertex is added to the graph and the number of vertices are increased by 1.
  RETURNS: The graph a.
-}

addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph vs edges) node = Graph (nub (node:vs)) edges

{- 1.3 -}
{-
  addEdge :: Eq a => Graph a -> (a,a) -> Graph a
  Adding an edge to a graph.
  PRE: v1 and v2 are vertices in the graph that aren’t already connected by an edge.
  POST: An edge connecting v1 and v2 is added to the graph and the number of edges are increased by 1.
  RETURNS: The graph a.
-}

addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (Graph vs edges) (v1,v2) | v1 /= v2 = Graph vs (edges ++ [(v1,v2)])
                                    | elem v1 vs = Graph vs (edges ++ [(v1,v2)])
                                    | elem v2 vs = Graph vs (edges ++ [(v1,v2)])
                                    | notElem (v1,v2) edges = Graph vs (edges ++ [(v1,v2)])
                                    | notElem (v2,v1) edges = Graph vs (edges ++ [(v1,v2)])
addEdge (Graph vs edges) node = Graph vs edges

{- 1.4 -}
{-
  Vertices :: Eq a => Graph a -> [a]
  Returns a list of all the vertices in a graph.
  PRE: Vertex is already in the graph.
  POST: The graph is unchanged.
  RETURNS: List of vertices.
-}

vertices :: Eq a => Graph a -> [a]
vertices (Graph vs edges) = vs

{- 1.5 -}
{-
  neighbors :: Eq a => Graph a -> a -> [a]
  Get a list of all the neighbors of a vertex in the graph.
  PRE: v1 and v2 are vertices in the graph.
  POST: The graph is unchanged.
  RETURNS: List of vertices.
-}

neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph vs []) node = []
neighbors (Graph vs ((v1,v2):edges)) node | v1 == node = v2 : neighbors (Graph vs edges) node
                                          | v2 == node = v1 : neighbors (Graph vs edges) node
neighbors (Graph vs (_:edges)) node = neighbors (Graph vs edges) node
