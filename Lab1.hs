--Lab 1 - Niklas Bergqvist
--Functional Programming

module Lab1(minus,fun1,fun2,fun3,fun4,fun5,fun6,sumSquareDiff) where

{- 1.1
    product 2 ->
    (if 2 == 1 then 1 else n*product(n-1)) ->
    (if false then 1 else n*product(n-1)) ->
    (2*product(2-1)) ->
    (2*(if 1 == 1 then 1 else n*product(n-1))) ->
    (2*(if true then 1 else n*product(n-1))) ->
    (2*(1)) ->
    2

   1.2

    product n
    product :: Int -> Int
    PRE:  n > 0
    RETURNS: the factorial of n
    POST: n!
    EXAMPLES:  product 1 = 1
               product 2 = 2
               product 3 = 6
               product 5 = 120
   1.3

   VARIANT: n
 -}

{- 2.1 -}

{-
  TYPE: Integer -> Integer -> Integer
  PRE: x >=0 and y >=0
  POST: x - y
  EXAMPLES: minus 1 1 = 0
            minus 3 1 = 2
            minus 7 4 = 3
-}

minus :: Integer -> Integer -> Integer
minus = \x y -> x - y

{- 2.2
    let foo = minus (5 :: Integer) 4

    -- foo is an Integer.

   2.3
    let bar = minus (5 :: Integer)

     -- bar is a function

   2.4

       minus 5 4
      --> (\x y -> x - y) 5 4
      --> (x-4) 5
      --> (5-4)
      --> 1
 -}

{- 3.1 -}
{-
    fun1
    TYPE: Integer -> Integer
    PRE: x >=0
    POST: x + 1
    EXAMPLES: fun1 3 = 4
              fun1 4 = 5
              fun1 10 = 11
-}

fun1 :: Integer -> Integer;
fun1 x = x + 1

{- 3.2 -}

{-
    fun2
    TYPE: Integer -> Integer -> Integer
    PRE: x & y >=0
    POST: x + y
    EXAMPLES: fun2 3 1 = 4
              fun2 4 2 = 6
              fun2 10 1 = 11
-}
fun2 :: Integer -> Integer -> Integer;
fun2 x y = x + y

{- 3.3 -}
-- ..
{-
     fun3
     TYPE: Integer -> (Integer, Integer)
     PRE: x >= 0
     POST: (x,x+1)
     EXAMPLES: fun3 1 = (1,2)
               fun3 0 = (0,1)
               fun3 6 = (6,7)
 -}

fun3 :: Integer -> (Integer, Integer);
fun3 x = (x, x+1)

{- 3.4 -}
-- ..
{-
 fun4
 TYPE: Integer -> (Integer, Integer)
 PRE: x&y any integer
 POST: x*y
 EXAMPLES:  fun4 (1,1) = 1
            fun4 (0,1) = 0
            fun4 (6,6) = 36
            fun4 (6,-3) = -18
-}
fun4 :: (Integer,Integer) -> Integer;
fun4 (x,y) = x*y

{- 3.5 -}
-- ..
{-
  fun5
  TYPE: Integer -> Double -> String -> String
  PRE: x >= 0, y >= 0.0 , z == String
  POST: "(x+y)z"
  EXAMPLES: fun5 1 2.0 "d" = "3.0d"
            fun5 5 2.0 "string" = "7.0string"
            fun5 0 3 "people" = "3.0people"
 -}
fun5 :: Integer -> Double -> String -> String;
fun5 x y z = show(fromIntegral x+y) ++ z

{- 3.6 -}
-- ..
{-
  fun6
  TYPE: Integer -> Double -> String -> String
  PRE: x >= 0, y == String, z  == String, n >=0
  POST: (x+n, "y+m")
  EXAMPLES: fun6 1  "people" "come" 2 = (3,"peoplecome")
            fun6 3  "electron" "microscope" 4 = (7,"electronmicroscope")
            fun6 0  "wiki" "pedia" 1 = (1,"wikipedia")
-}
fun6 :: Integer -> String -> String -> Integer -> (Integer, String);
fun6 x y z n = (x*n, y++z)

{- 4 -}

-- use helper functions as needed
-- ..
{-
 sumSquareDiff
 TYPE: integer -> integer
 PRE: n>=0
 POST: sum(1+2+3+,..,+n)^2 - sum(1^2+2^2+3^2+,...,+n^2)
 EXAMPLES: sumSquareDiff 0 = 0
           sumSquareDiff 2 = 4
           sumSquareDiff 3 = 22
           sumSquareDiff 4 = 70
           sumSquareDiff 10 = 2640
-}

sumSquareDiff :: Int -> Int
sumSquareDiff n = if n < 1 then 0 else ((sum [1..n]^2) - (sum [x^2 | x <- [1..n]]))
