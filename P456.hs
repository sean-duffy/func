-- Example solutions for FUNC exercises after lectures 4-6, except for
-- the complete programs Edit and PrettyHa which are separate.

data Tri a  =  Tri {trows :: [[a]]}

pascal :: Tri Integer
pascal  =  Tri (iterate rowAfter [1])
  where
  rowAfter r  =  [1] ++ zipWith (+) r (tail r) ++ [1]
 
-- HAMMING
-- This solution closely follows the hint given in the question.
-- Only one equation is needed for merge: since both arguments are
-- infinite lists there is no need to cater for [].

hamming :: [Integer]
hamming  =  h 2 `merge` h 3 `merge` h 5
  where
  h n    =  map (n*) (1 : hamming)
  
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)  =  case compare x y of
                        LT -> x : merge xs (y:ys)
                        EQ -> x : merge xs ys
                        GT -> y : merge (x:xs) ys

-- PRIMES
-- A common definition loosely based on the "Sieve of Eratosthenes"
-- introduces for each prime a new filter to eliminate its products.
-- This solution is much faster, as there is only one filter.

primes :: [Integer]
primes  =  sieve [2..]
  where
  sieve (p:xs)         =  p : filter (noFactorIn primes) xs
  noFactorIn (p:ps) x  =  p*p > x || x `mod` p > 0 && noFactorIn ps x

-- QUEENS
-- This solution eliminates the separate safety test on combinations
-- of q and b.  Instead, constraints are represented in auxiliary
-- arguments of function queens': the first lists ranks not yet
-- occupied; the second and third list ranks reached along rising
-- and falling diagonals from other queens.  As a side-benefit
-- queens now finds all n by n solutions for any non-negative n.

queens :: Int -> [[Int]]
queens n  =  queens' [1..n] [] []
  where
  queens' []  _  _   =  [[]]
  queens' ms  dr df  =  [ q:b | (q,etc) <- picks ms,
                                q `notElem` dr, q `notElem` df,
                                b <- queens' etc
                                       [r+1 | r <- q:dr, r < n]
                                       [f-1 | f <- q:df, f > 1] ]

picks :: [a] -> [(a,[a])]
picks []      =  []
picks (x:xs)  =  (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]
