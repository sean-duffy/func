import System.Environment
import BreadthFirst
import BatchedQ

main :: IO ()
main = do [n] <- getArgs
          putStrLn (show (countTo (read n)))

countTo :: Int -> [Int]
countTo n = take n (breadthFirst (\n -> [((2*n)+1),2*(n+1)]) 0)
