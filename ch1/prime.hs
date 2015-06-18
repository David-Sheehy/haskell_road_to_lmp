-- The prime test aka section 1.2

-- Does d evenly divide into n
-- aka numerator mod denomitor == 0
divides :: Integer -> Integer -> Bool
divides d n = n `rem` d == 0

-- Calculates the lowest denominator for the number n.
-- Assumes that n is positive
ld :: Integer -> Integer
ld n = ldf 2 n

-- Finds the least dividing factor for n starting at k
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (succ k) n
