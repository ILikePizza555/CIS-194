-- Rules:
-- 1. Must include an explanation
-- 2. Comments, type signatures, imports, and whitespace don't count
-- 3. Must avoid partial functions (unless they are properly typed, i.e safeHead :: [a] -> Maybe a)

module CodeGolf where

-- Returns a list every nth element of a list
-- The function works by recursively dropping n-1 elements of the given list
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
    (y:ys) -> y : every n ys
    [] -> []

-- Returns a list of lists where the nth list is a list of every n elments from the souce list
-- The function works by partially applying every to the source list, and then mapping it to a list 
-- of numbers from 1 to the length of the source list
skips :: [a] -> [[a]]
skips xs = map (flip every xs) [1..(length xs)]

-- Returns all the local maxima in a list of integers
-- Works by recursively pattern-matching against triples
localMaxima :: [Int] -> [Int]
localMaxima (a:b:c:xs)
    | b > a && b > c = b : localMaxima xs
    | otherwise      = localMaxima (b:c:xs)
localMaxima _ = []