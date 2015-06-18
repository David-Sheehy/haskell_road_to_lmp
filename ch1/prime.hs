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


prime0 n | n < 1     = error "not a positive number."
         | n == 1    = False
         | otherwise = ld n == n

-- Section 1.7
-- prime factorization
-- Calculate the prime factors of n
factors :: Integer -> [Integer]
factors n | n < 1  = error "argument not positive" 
          | n == 1 = []
          | otherwise = p : factors (n `div` p) where p = ld n

ldp :: Integer -> Integer
ldp = ldfs primes1

ldfs :: [Integer] -> Integer -> Integer
ldfs (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldfs ps n

-- generate an infinite list of primes. Lazy evaluation lets this be possible
-- and actually usable.
primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime n | n < 1     = error "not a positive integer."
        | n == 1    = False
        | otherwise = ldp n == n
