module Practical2 where
import Parse

data Tri a = Tri {trows :: [[a]]}

instance Show a => Show (Tri a) where
  show (Tri rs)   =  unlines (zipWith (++) spaces (map (unwords . map show) rs))
    where spaces  =  map (map (const ' ')) (reverse rs)

pascal :: Tri Integer
pascal = Tri ([1] : map (\r -> pairf (+) ([0] ++ r ++ [0])) (trows pascal))

pairf :: (a -> a -> a) -> [a] -> [a]
pairf f [x, y] = [f x y]
pairf f (x:y:xs) = (f x y) : (pairf f (y:xs))

--hamming :: [Integer]
--hamming 

mul :: Integer -> [Integer]
mul n = [n*2, n*3, n*5]

-- Ha!
data Prog = Prog [Eqn]
data Eqn = Eqn Name [Pat] Exp
data Pat = PNil | PVar Name | PCons Name Name
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
type Name = String

--prog :: Parser Prog
--prog = name

name :: Parser Name
name = (many1 (lower .|. upper))

nil :: Parser Exp
nil = (char "[") .+. (char "]")

--arg :: Parser Exp
--arg = 
