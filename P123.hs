-- Example solutions for FUNC exercises after lectures 1-3
-- except for the Ktrain program which is separate.

import Prelude hiding (take, drop, zipWith)

-- TAKE, DROP
-- There are shorter solutions that depend on equation
-- order, but this one doesn't.

take :: Int -> [a] -> [a]
take 0 _       =  []
take _ []      =  []
take n (x:xs)  |  n > 0
               =  x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs      =  xs
drop _ []      =  []
drop n (_:xs)  |  n > 0
               =  drop (n-1) xs

-- POSITIONS
-- The required function numbers positions starting from 1.
-- A straightforward generalisation numbers them starting
-- from s, which is introduced as an auxiliary argument.

positions :: Eq a => [a] -> a -> [Int]
positions xs i  =  posStarting 1 xs
  where
  posStarting _ []      =  []
  posStarting s (x:xs)  =  if x==i then s:p else p
    where
    p  =  posStarting (s+1) xs

-- DUPLICATES
-- There are lots of other ways to solve this one - nearly all
-- of them more complicated!

duplicates :: Eq a => [a] -> [a]
duplicates []      =  []
duplicates (x:xs)  =
  if not (x `elem` d) && x `elem` xs then x:d else d 
  where
  d  =  duplicates xs

-- SORT
-- Perhaps the simplest solution is an insertion sort.  It is reached
-- by asking the usual recursive question, and fits the foldr pattern.

sort :: Ord a => [a] -> [a]
sort  =  foldr insert []
  where
  insert n []      =  [n]
  insert n (x:xs)  =  if n<=x then n: x: xs else x: insert n xs

-- ZIPWITH
-- Putting the recursive equation first allows all the base cases to
-- be combined in a single equation.  But this does rely on equation
-- order.

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys)  =  f x y : zipWith f xs ys
zipWith f _      _       =  []

-- MAT SHOW, TRANSPOSE
-- We have suitable auxiliary power already defined in the Prelude:
-- map, unlines and unwords for showing matrices;
-- map and zipWith for transpose.

data Mat a  =  Mat {mrows :: [[a]]}

instance Show a => Show (Mat a) where
  show  =  unlines . map (unwords . map show) . mrows

transpose :: Mat a -> Mat a
transpose  =  Mat . trans . mrows
  where
  trans [r]     =  map (:[]) r
  trans (r:rs)  =  zipWith (:) r (trans rs)

-- The Triangle type is just a synonym for a list of lists but signals
-- the intention that the nth inner list has n elements.

-- TRI SHOW, TROL, TROR
-- The Show instance for Tri is like the one for Mat but
-- with the added complication of tapering indentation.

data Tri a = Tri {trows :: [[a]]}

instance Show a => Show (Tri a) where
  show (Tri rs)   =  unlines (zipWith (++) spaces (map (unwords . map show) rs))
    where spaces  =  map (map (const ' ')) (reverse rs)

-- A non-empty triangle decomposes into edge + smaller triangle.
-- The frustum accumulator f in trolOnto acts as a platform for
-- the triangle to rotate onto.

trol :: Tri a -> Tri a
trol = Tri . trolOnto [] . trows
  where
  trolOnto f []      =  f
  trolOnto f (r:rs)  =  trolOnto (map head (r:rs) : f) (map tail rs)

-- We use the inverse pattern of computation in frorInto:
-- the accumulator t is now a triangle and the recursively
-- decomposed argument is a frustum.

tror :: Tri a -> Tri a
tror = Tri . frorInto [] . trows
  where
  frorInto t []      =  t
  frorInto t (r:rs)  =  frorInto (zipWith (:) r ([]:t)) rs

-- SUBLISTS
-- There are two ways of using each sublist of the tail:
-- (1) any sublist of xs is a subset of (x:xs) also, and
-- (2) it will remain so if x is added to it.

sublists :: [a] -> [[a]]
sublists []      =  [[]]
sublists (x:xs)  =  s ++ map (x:) s
  where
  s  =  sublists xs
 
-- SEGMENTS
-- One definition of a segment is the prefix of a suffix.  Using a
-- comprehension gives a one-liner to list them all.

segments :: [a] -> [[a]]
segments xs  =  [p | s <- suffixes xs, p <- prefixes s]

prefixes :: [a] -> [[a]]
prefixes []      =  []
prefixes (x:xs)  =  [x] : map (x:) (prefixes xs)

suffixes :: [a] -> [[a]]
suffixes []      =  []
suffixes (x:xs)  =  (x:xs) : suffixes xs

-- PERMS
-- There are many possible definitions.  The nicest are purely
-- structural solutions: they do not require comparisons of elements.
-- The picks function here lists all ways of picking out one element
-- from a list, each giving an (element,list-remaining) pair.

perms :: [a] -> [[a]]
perms []      =  [[]]
perms xs      =  [x:p | (x,xs') <- picks xs, p <- perms xs']

picks :: [a] -> [(a,[a])]
picks []      =  []
picks (x:xs)  =  (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]

-- PARTS
-- A partition p of non-empty xs can be extended to a partition of (x:xs)
-- in just two ways:
-- (1) add [x] as a segment at the front of p
-- (2) add x to the start of the first segment in p

parts :: [a] -> [[[a]]]
parts []      =  []
parts [x]     =  [[[x]]]
parts (x:xs)  =  [p' | p@(ys:etc) <- parts xs, p' <- [[x]:p, (x:ys):etc]]

-- CHANGE
-- This solution drops the coins into each bag in ascending order
-- of value.  The comprehension chooses a first coin value c, which
-- clearly cannot be more than m. The rest of the bag is filled
-- with some recursively computed combination of coins (>= c) making
-- up the outstanding amount (m-c).

change :: [Int] -> Int -> [[Int]]
change cs 0  =  [[]]
change cs m  =  [c : b | c <- cs, c <= m,
                         b <- change (filter (>= c) cs) (m-c)]
