module Lists where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [] a
--    = []
--    | (:) a [a]
--
-- This is equivalent to:
--
-- data List a
--    = Nil
--    | Cons a (List a)

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length = undefined

-- | Compute the sum of an integer list.
sum = undefined

-- | Compute the product of the elements in an integer list.
product = undefined

-- | Are all of the integers in this list odd?
allOdd = undefined

-- | Double all the elements in an integer list.
doubleAll = undefined

-- | Flip all of the boolean values in a boolean list.
notAll = undefined

-- | Apply the even function to all elements in the list.
evenAll = undefined


----------------------------
-- Higher-Order Functions --
----------------------------


-- * Map

-- | Map a function over the elements in a list.
map = undefined

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = undefined

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = undefined

-- | Reimplement evenAll using map.
evenAll' :: [Int] -> [Bool]
evenAll' = undefined


-- * Fold

-- | Fold a function over the elements in a list.
foldr = undefined

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = undefined

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = undefined

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = undefined

-- | Reimplement allOdd using foldr.
allOdd' :: [Int] -> Bool
allOdd' = undefined

-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = undefined
