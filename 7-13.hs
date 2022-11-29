--                      LAZINESS

f :: [Int] -> [Int] -> Int
f (a:as) (b:bs) = a + b
----------------------------------------------
primes :: [Int] -> [Int]
primes (a:b) = a : primes [x | x <- b, mod x a /= 0]
---------------------------------------------