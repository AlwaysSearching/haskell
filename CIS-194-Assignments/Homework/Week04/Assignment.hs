module Homework.Week04.Assignment (
    fun1,
    fun2,
    testFun1,
    testFun2,
    foldTree,
    xor,
    map',
    sieveSundaram
) where

import Data.List


-- Exercise 1: Wholemeal Programming
-- reimpliment recursive functions using wholemeal programming style -> utilize the core library
fun1 :: [Integer] -> Integer
fun1 = foldl' (*) 1 . map (subtract 2) . filter even 

fun2 :: Integer -> Integer
fun2 = foldl' (+) 0 . filter even . takeWhile (/=0) . iterate step

step :: Integer -> Integer
step x | x == 1    = 0
       | even x    = div x 2
       | otherwise = 3*x + 1

-- Original implimentation in the assignment. used to test the above.
testFun1 :: [Integer] -> Integer
testFun1 [] = 1
testFun1 (x:xs)
    | even x    = (x-2) * testFun1 xs
    | otherwise = testFun1 xs

testFun2 :: Integer -> Integer
testFun2 1 = 0
testFun2 x
    | even x    = x + testFun2 (div x 2) 
    | otherwise = testFun2 (3*x + 1)

-- Exercise 2: Tree folding Fun!

data Tree a  = Leaf
             | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertInTree Leaf

insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 Leaf x Leaf
insertInTree x (Node h left y right)
    | hLeft < hRight   = Node h           nLeft y right  -- left subtree is smaller than right -> add to left 
    | hLeft > hRight   = Node h           left  y nRight -- right subtree is smaller than left -> add to right 
    | nhLeft < nhRight = Node h           nLeft y right  -- left and right subtrees are equal height. Add to tree with fewer nodes
    | otherwise        = Node (nhRight+1) left  y nRight -- the tree is full -> add another level at the leaves
  where hLeft   = heightTree left
        hRight  = heightTree right
        nLeft   = insertInTree x left
        nRight  = insertInTree x right
        nhLeft  = heightTree nLeft
        nhRight = heightTree nRight

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node height _ _ _) = height

showTree :: Show a => Tree a -> String
showTree Leaf = ""  
showTree n@(Node i _ _ _) = go i n
  where
    go _ (Leaf) = "" 
    go i (Node _ l c r) = go (i-1) l ++ 
        replicate (4*fromIntegral i) ' ' ++ show c ++ "\n" ++ go (i-1) r


-- Exercise 3: More Folds!
-- Impliment the given functions utilizing folding

-- filter out the odd values, use foldl to get list length, check parity.
xor :: [Bool] -> Bool
xor = odd . foldl' (\acc _ -> acc + 1) 0 . filter id

-- from right to left, add the mapped value to the front of the accumulator list.
-- Note, no seperate case is needed for the case a null list is passed, since foldr handles this properly.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\y ys -> (f y):ys) [] 

-- reimpliment filter for fun d: same idea as above, but do not append if function evaluates to false
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x:xs else xs) []


-- Exercise 4: Finding Primes
-- Impliment the sieve of Sundaram

-- filter numbers with list comprehension. map the resulting list of numbers to generate the resulting primes.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\i -> 2*i+1) $ [j | j <- [1..n], not $ elem j numbers] 
  where numbers = numbersToRemove n

numbersToRemove :: Integer -> [Integer]
numbersToRemove = flip takeWhile (map (\(a,b) -> a+b+2*a*b) [(i,j) | i<-[1..], j<-[1..]]) . belowN

-- enable pointfree style by defining a boolean function with the integer passed into the sieve.
belowN :: Integer -> (Integer -> Bool)
belowN n = (<n)


-- Impliment Max Subarray Sum 
maxSubArray :: [Integer] -> Integer
maxSubArray xs = snd $ foldl' next_pair (0, 0) xs

next_pair :: (Integer, Integer) -> Integer -> (Integer, Integer)
next_pair (c_sum, b_sum) x = (next_sum, max b_sum next_sum)
  where next_sum = max 0 (c_sum + x)
