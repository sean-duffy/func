module BreadthFirst where
import BatchedQ
import Prelude hiding (head)

breadthFirst :: (a -> [a]) -> a -> [a]
breadthFirst b r  =  bf b (queue [r] :: Queue a)

--bf :: (a -> [a]) -> [a] -> [a]
--bf b []      =  []
--bf b (x:xs)  =  x : bf b (xs ++ b x)

bf :: (a -> [a]) -> Queue a -> [a]
bf b q  =  if isEmpty q then items q
           else snoc (bf b (snoc q (b x))) x
             where x = head q
