module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
-- define function nthList: construct list of nth elements. 
-- from right to left - zip list with the [1,2,..n,1,..,n,..] (infinite list), filter out elements where fst (i, x) '==' n, and take the second element
-- for each value 1 to length of the passed list, call nthList on the given list xs.
skips :: [a] -> [[a]]
skips xs = [nthList n xs | n <- [1..length xs]]
  where nthList n = (map snd) . filter ((==n) . fst) . zip (cycle [1..n]) 

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x<y && z<y = y:localMaxima (y:z:zs)
    | otherwise  = localMaxima (y:z:zs)
localMaxima _ = []


-- #3
-- use countElem to get element counts. for max count plus 1 down to 1, create line of stars.
-- use unlines to concat the linst of strings to one string with new line connecting the list elements.
histogram :: [Integer] -> String
histogram xs =
    let cnts = countElem xs
        mxm = maximum c 
    in unlines (map (stars cnts) [mxm+1,mxm..1]) ++ "==========\n0123456789\n"
                   
-- count occurences with a list comprehension
countElem :: [Integer] -> [Int]
countElem xs = [length $ filter (==n) xs | n <- [0..9]]

-- add star to line in element count is gte value n
stars :: [Int] -> Int -> String
stars xs n = [if n <= x then '*' else ' ' | x <- xs]
