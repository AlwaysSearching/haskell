-- Exercise #1
-- function that checks if an element is present in a given list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = (a==x) || (elem' a xs)

-- Exercise 2
-- remove duplicates from a given list
nub' :: (Eq a) => [a] -> [a] 
nub' [] = []
nub' (x:xs) 
 | elem' x xs = nub' xs
 | otherwise = x : nub' xs

-- Exercise 3
-- validate if a list is in ascending order
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:xs) = (x <= (head xs)) && isAsc xs 

-- Exercise 4
-- Check if a path between two nodes exist within a given graph. 
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] a b = a==b
hasPath xs a b 
 | a==b = True
 | otherwise = or [hasPath xs' b' b | (a', b') <- xs, a'==a]
 where xs' = [(x,y) | (x,y)<-xs, x/=a]
