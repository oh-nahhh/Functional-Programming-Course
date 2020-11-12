{-
  Functional Programming 1
  Assignment 3
  Niklas Bergqvist
 -}

module Lab3(Fruit(Apple,Banana,Lemon),sumPrice,BSTree(Void,BSNode),subTree,Tree(Node),count,labels,height,(++),elem,last,reverse,filter) where

import Prelude hiding ((++),elem,last,reverse,filter)

{- 1.1 -}
--
data Fruit = Apple Double | Banana Double | Lemon Int

{- 1.2 -}
-- ..
sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
sumPrice [] _ _ _ = 0
sumPrice ((Apple a) : xs) apple_price banana_price lemon_price = a*apple_price + sumPrice xs apple_price banana_price lemon_price
sumPrice ((Banana a) : xs) apple_price banana_price lemon_price = a*banana_price + sumPrice xs apple_price banana_price lemon_price
sumPrice ((Lemon a) : xs) apple_price banana_price lemon_price = (fromIntegral a) * lemon_price + sumPrice xs apple_price banana_price lemon_price

{-
sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
DESCRIPTION: Take a list of fruits with the price of apples, bananas, and the price of lemons (per unit), and return the total cost of the
items in the list.
PRE: A list which should contain the type fruit.
POST : The total cost of all fruits.
VARIANT: The length of fruit list.
EXAMPLES: sumPrice [] 1.0 1.0 1.0 = 0.0
          sumPrice [Apple 3.0] 1.0 2.0 3.0 = 3.0
          sumPrice [Apple 3.5, Banana 4.5, Lemon 6] 5.0 9.0 11.0 124.0
-}

{- 2 -}

{- Binary search trees

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
       -}

data BSTree = Void | BSNode BSTree Integer BSTree -- do not modify this line

-- ..
subTree :: Integer -> Integer -> BSTree -> BSTree
subTree _ _ Void = Void
subTree a b (BSNode left label right)
 | label < a = subTree a b right
 | label >= b = subTree a b left
 | label >= a && label < b = (BSNode (subTree a b left)  label  (subTree a b right))

{-
subTree a b (left label right)
TYPE: Integer -> Integer -> BSTree -> BSTree
DESCRIPTION: Function for representing a Binary Search Tree.
PRE: True.
POST : A subtree with only the nodes in the tree that are >= to a and smaller than b
VARIANT: The number of unchecked nodes in the initial tree.
EXAMPLES: t = BSNode ( BSNode ( BSNode Void 0 ( BSNode Void 2 Void ) ) 3 ( BSNode Void 5 Void ) ) 6 ( BSNode Void 7 ( BSNode Void 8 ( BSNode Void 9 Void ) ) )
          subTree 5 8 t == BSNode (BSNode Void 5 Void) 6 (BSNode Void 7 Void)
          subTree 10 20 t == []
-}

{- 3.1 -}
-- ..
data Tree a = Node a [Tree a] deriving (Show)

{- 3.2 a) -}
-- ..
count :: Tree a -> Integer
count (Node a []) = 1
count (Node a b) = (sum . map count $ b) + 1

{-
TYPE: Tree a −> Integer
DESCRIPTION: Computes the number of nodes in a tree.
PRE: True.
POST: The number of nodes in the Tree.
EXAMPLES: count (Node 1[]) == 1
          count (Node "hej"[]) == 1
          count (Node 1 [Node 2 []]) == 2
          count (Node 1 [Node 2 [Node 3 []]]) == 3
-}

{- 3.2 b) -}
-- ..
labels:: Tree a -> [a]
labels (Node a []) = [a]
labels (Node a b) = [a] ++ (concat . map labels $ b)

{-
TYPE: Tree a -> [a]
DESCRIPTION: Compute the list of all node labels in  a tree.
PRE: True.
POST: List of nodes in the tree.
EXAMPLES: labels (Node 1 []) == [1]
          labels (Node 1 [Node 2 []]) == [1,2]
          labels (Node 1 [Node 2 [Node 3 []]]) == [1,2,3]
          labels (Node "hej"[]) == ["hej"]
-}

{- 3.2 c) -}
-- ..
height:: Tree a -> Integer
height (Node a []) = 1
height (Node a b) = (maximum . map height $ b) + 1
{-
TYPE:  Tree a -> Integer
DESCRIPTION: Compute the height of a tree.
PRE: True.
POST : The height of the Tree.
EXAMPLES: height (Node "hej"[]) == 1
          height (Node 1 [Node 2 [Node 3 []]]) == 3
          height (Node 1 [Node 2 []]) == 2
          height (Node 1 []) == 1
-}

{- 4.1 -}
-- ..
(++)::[a] -> [a] -> [a]
(++) x y = foldr (:) y x
{-
Type: [a] -> [a] -> [a]
DESCRIPTION: Concat two lists.
PRE: Two lists with same type.
POST: Concatenated list of x and y.
EXAMPLES: [] ++ [1,2] ==  [1,2]
          [3,2] ++ [] == [3,2]
          [1,7,3,4,5] ++ [3,4,10,1] [1,7,3,4,5,3,4,10,1]
-}

{- 4.2 -}
-- ..
elem :: Eq a => a -> [a] -> Bool
elem x list = foldl (\a b -> a || (x == b)) False list

{-
TYPE: Eq a => a -> [a] -> Bool
DESCRIPTION: Take an element and a list, return true if the element is in the list.
PRE: Lists of the same type.
POST : True if the element in the list.
EXAMPLES: elem 'a' ['b']" == False
          elem 1 [1,2,3] == True
          elem 5 [1,2,3,4,200,100,5] == True
-}

{- 4.3 -}
-- ..
last :: [a] -> a
last list = foldl1 (\x y-> y)list
{-
DESCRIPTION: The last element in a list.
PRE: True.
POST: the last element in the list
EXAMPLES: last ['a','b','c'] == 'c'
          last [1,2,3,4,200,100,5] = 5 (last [1,2,3,4,200,100,5])
          last [\"X\"] == "X"

-}

{- 4.4 -}
-- ..
reverse:: [a] -> [a]
reverse x = foldl (\a y -> y : a) [] x
{-
TYPE: [a] -> [a]
DESCRIPTION: Reversing a list.
PRE: A list
POST: A list in reversed order.
EXAMPLES: reverse [1,2,3,4,5] == [5,4,3,2,1]
          reverse [\"X\"] == ["X"]
-}

{- 4.5 -}
-- ..
filter ::(a -> Bool) -> [a] -> [a]
--filter p [] = []
filter p xs = foldr (\x xs -> if p x then x:xs else xs ) [] xs
{-
TYPE:(a -> Bool) -> [a] -> [a]
DESCRIPTION: A list constructed from members of a list (the second argument) fulfilling a condition given by the first argument.
PRE: True.
POST : A list of elements which satisfy the argument.
EXAMPLES: filter (>5) [1,2,3,4,5,6,7,8] == [6,7,8]
          filter (<0) [1,2,3,4,200,100,5] == []

-}
