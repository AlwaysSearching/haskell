module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n 
    | n <= 0	= []
    | otherwise = toDigits (div n 10) ++ [mod n 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits 

-- . is chained assignment so we reverse the list, zipWith on an alternating list and reverse again.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) alternate . reverse
    where alternate = 1 : 2 : alternate

-- apply sum (toDigits x) to each x in the list. then sum over the resulting list.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- #4
validate :: Integer -> Bool
validate n = (0 < n) && mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n <= 0 = []
    | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a 

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src t1 t2 goal
    | n <= 0	= []
    | n == 1	= [(src,goal)]
    | n == 3	= [(src,t1), (src,t2), (src,goal), (t2,goal), (t1,goal)]
    | otherwise = (hanoi4 k src t2 goal t1) ++ (hanoi4 (n-k) src t2 t1 goal) ++ (hanoi4 k t1 src t2 goal)
    where k = n - (round.sqrt.fromInteger $ (2*n + 1)) + 1
{-- 
    hanoi4 k src t2 goal t1 	    -> Transfer top k disks to the second peg
    hanoi4 (n-k) src t1 t2 goal)    -> Transfer remaining pegs onto the target peg
    hanoi4 k t1 src t2 goal)	    -> Transfer top k pegs from peg 2 onto the target peg
--}
