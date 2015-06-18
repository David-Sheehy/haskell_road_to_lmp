-- Section 1.5 Playing the Haskell Game
mnmInt :: [Integer] -> Integer
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min' x (mnmInt xs)

min' :: Integer -> Integer -> Integer
min' x y | x <= y    = x
         | otherwise = y

srtInts' :: [Integer] -> [Integer]
srtInts' [] = []
srtInts' xs = let
                m = mnmInt xs
              in m : (srtInts' (removeFst m xs))

average :: [Integer] -> Float
average [] = error "emtpy list"
average xs = fromInteger (sum' xs) / fromInteger (length' xs)

-- Calculate the sum of a list
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Calculate the length of a list
length' [] = 0
length' (x:xs) = 1 + length' xs



-- check if str1 is a prefix of str2
prefix :: String -> String -> Bool
prefix [] ys     = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-- Exercise functions

-- 1.9
maxInt :: [Integer] -> Integer
maxInt []    = error "empty list"
maxInt [x]   = x
maxInt (x:xs)= max x (maxInt xs)

-- 1.10: Define a function removeFst that removes the first occurence of an
-- integer m from a list of integers. If m does not occur int he list, the list
-- remains unchanged.
removeFst m []    = []
removeFst m [x]   | m == x    = []
                  | otherwise = [x]

removeFst m (x:xs)| m == x    = xs 
                  | otherwise = x : (removeFst m xs)

-- 1.13 count
count l [] = 0
count l [c]    | l == c    = 1
               | otherwise = 0
count l (c:cs) | l == c = 1 + (count l cs)
               | otherwise = count l cs

-- 1.14
-- Iteritively duplicate the characters in a string
blowup :: String -> String
blowup [] = error "empty string"
blowup cs = blowup' 1 cs
blowup' n [c] = replicate n c
blowup' n (c:cs) = replicate n c ++ (blowup' (succ n) cs)


-- 1.15
-- A selection sort algorithm.
srtString :: [String] -> [String]
srtString [s]    = [s]
srtString (ss)   = let 
                     s = minStr ss
                   in  s : srtString (removeFst s ss)

-- helper function for the srtString
-- Returns the lowest alphabetic string in an array
minStr :: [String] -> String
minStr []     = error "empty list"
minStr [s]    = s
minStr (s:ss) = min s (minStr ss)


-- 1.17:
-- Check if str1 is a substring of str2
substring :: String -> String -> Bool
substring [] [] = False
substring xs [] = False
substring xs ys | xs `prefix` ys        = True
                | length xs > length ys = False
                | otherwise             = substring xs (tail ys)
