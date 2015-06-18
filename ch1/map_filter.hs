-- section 1.8
-- examples

-- Applies a function to each element in a list
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs


-- Removes elements in a list that return true on a function.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise = filter' p xs
-- exercises

-- exercise 1.20
-- write a function that takes a lists of lists and returns a list of their
-- lengths.
lengths :: [[a]] -> [Int]
lengths [] = []
lengths xs = map length xs

-- exercise 1.21
-- Use map to write a function sumLengths that takes a list of lists and
-- returns the sum of their lengths
sumLength :: [[a]] -> Int
sumLength [] = 0
sumLength xs = sum (map length xs) -- sum (lengths xs)
