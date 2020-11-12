{-
  Functional Programming 1
  Assignment 2
  Niklas Bergqvist
 -}

module Lab2(iota,inter,interOrdered,isMatch) where

{- 1
    1. head :: [a] -> a
    2. tail :: [a] -> [a]
    3. \x -> x :: p -> p
    4. (,) :: a -> b -> (a,b)
    5. (:) :: a -> [a] -> [a]
    6. [[]] :: [[a]]
    7. tail [[]] :: [[a]]
    8. id : []  :: [a -> a]
    9. id id :: a -> a
   10. head [id] "foo" :: [Char]

   All of them are polymorphic expressions except the last one, which only takes the type char.
 -}

--
{-
iota n
TYPE: Int -> [Int]
DESCRIPTION: Take an interger and makes a list from 0 to n-1.
PRE: n>=0
RETURNS : A list of 0 to n-1.
EXAMPLES:  iota 0 = []
           iota 1 = [0]
	         iota 3 = [0,1,2]
           iota 5 = [0,1,2,3,4]
iota :: Int -> [Int]
VARIANT: n
-}

iota :: Int -> [Int]
iota n = [0..n-1]

{- 3.1 -}

{-
inter s1 s2
DESCRIPTION: Take the intersection of two lists (s1 & s2) and return a new list of elements that are in s1 and s2.
PRE: Two lists with same type.
RETURNS : A list of the elements that are in both s1 s2 ordered by appearance.
EXAMPLES: inter [2,1] [1] = [1]
          inter [1] [1,2] = [1]
          inter [2,1] []  = []
          inter [2, 1, 3] [1, 2] = [2, 1]
inter :: Eq a => [a] -> [a] -> [a]
VARIANT: Length of the list.
-}

inter :: Eq a => [a] -> [a] -> [a]
inter [] _ = []
inter (x:s) l | elem x l = x : inter s l
               | otherwise = inter s l

{- 3.2 -}
interOrdered :: Ord a => [a] -> [a] -> [a]
interOrdered _ [] = []
interOrdered [] _ = []
interOrdered (x:s1) (y:s2) |  (x==y) = x : interOrdered s1 s2
                           |  (x < y) = interOrdered s1 (y:s2)
                           |  (y < x) = interOrdered (x:s1) s2

{-
interOrdered s1 s2
TYPE: Ord a => [a] -> [a] -> [a]
DESCRIPTION: Intersects two lists in ascending order.
PRE: Two lists of the same type.
RETURNS : An ordered list of elements in both s1 s2.
EXAMPLES: interOrdered [1,4] [1,3] = [1]
          interOrdered [1,2,3,4] [0,1,7] = [1]
          interOrdered [1,2,3,4] [2,4,5] = [2,4]
          interOrdered [3,4] [3,4] = [3,4]
VARIANT: Length of the list s1 + length of s2.
-}

{- 3.3 -}

s1 = iota 100000
s2 = iota 1000000

t1 = inter s1 s2
t2 = interOrdered s1 s2

{-
*Lab2> length s1
100000
(0.04 secs, 8,056,864 bytes)
*Lab2> length s2
1000000
(0.08 secs, 80,055,224 bytes)
*Lab2> length t2
100000
(0.09 secs, 26,454,512 bytes)
*Lab2> length t1
100000
(55.60 secs, 20,054,448 bytes)
-}

{- 4 -}
isMatch :: [Char] -> [Char] -> Bool
isMatch (x:s1) (y:s2)
  | y == '*' = isMatch str s2 || isMatch s1 pattern
  | x == y || y == '?' = isMatch s1 s2
  | otherwise = False
  where str = x:s1
        pattern = y:s2
isMatch [] pattern = all (=='*') pattern
isMatch (x:s1) [] = False


{-
isMatch::[Char] -> [Char] -> Bool
isMatch s1 s2
DESCRIPTION: It takes two ordered two lists and check if they match.
PRE: The lists must be of the same type.
RETURNS : True if they are the same otherwise it returns false.
EXAMPLES: (isMatch "aa" "a") == False
          (isMatch "aa" "*") == True
          (isMatch "adceb" "*a*b") == True
          (isMatch "acdcb" "a*c?b") == False
-}
