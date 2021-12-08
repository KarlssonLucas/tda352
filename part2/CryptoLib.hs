module CryptoLib (eea, modExp, modInv, fermatPT) where

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea (a, b) = calculateEEA (a, b, a `div` b, a `mod` b, 1, 0, 1, 0, 1, - (a `div` b) )

calculateEEA :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int)
calculateEEA (_,b,_,0,_,s2,_,_,t2,_) = (b,s2,t2)
calculateEEA (a, b, q, r, s1, s2, s3, t1, t2, t3) = calculateEEA (b, r, q, b `mod` r, s2, s3, s2 - (q * s3), t2, t3, t2 - (q * t3)) where q = (b `div` r) :: Int

-- | Returns a^k (mod n).
modExp :: (Int, Int, Int) -> Int
modExp (_,_,0)   = 1
modExp (n, a, k) | even k    = (rem^2) `mod` n 
                 | otherwise = (a * rem^2) `mod` n
                 where rem = modExp (n, a, k `div` 2)

-- | Returns the value v such that n*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Int, Int) -> Int
modInv (n, m) | gcd /= 1 = 0
              | otherwise = s `mod` m
             where (gcd, s, t) = eea (n,m)

-- | Returns 0 if n is a Fermat Prime, otherwise it returns the lowest
-- Fermat Witness. Tests values from 2 (inclusive) to n/3 (exclusive).
fermatPT :: Int -> Int
fermatPT n = fermatHelper n 2

fermatHelper :: Int -> Int -> Int
fermatHelper a k | modExp (a, k, a-1) == 1 = fermatHelper a (k+1)
                 | k >= a `div` 3 = 0
                 | otherwise = k

 