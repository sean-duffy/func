module Practical1 where
import Prelude hiding (take, drop, zipWith)

take :: Int -> [a] -> [a]
take _ [] = []
take 1 (x:_) = [x]
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 1 (_:xs) = xs
drop n (_:xs) = drop (n-1) xs

positions :: Eq a => [a] -> a -> [Int]
positions [] _ = []
positions (x:xs) e = 
  if e == x then 0 : tail
  else tail
  where tail = map (+1) (positions xs e)

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates [x] = []
duplicates (x:xs) = 
  if elem x xs && not (elem x tail) then x : tail
  else tail
  where tail = duplicates xs

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

data Mat a = Mat [[a]]

instance Show a => Show (Mat a) where
  show (Mat m) = unlines (map (unwords . (map show)) m)

transpose :: Mat a -> Mat a
transpose (Mat [[]]) = Mat [[]]
transpose (Mat (x:xs)) 
  = zipWith (++) (map (\n -> [n]) x) (transpose (Mat [xs]))
