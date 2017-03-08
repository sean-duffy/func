import System.Environment

main :: IO (Int, Int)
main = do [fileName] <- getArgs
          inputText <- readFile fileName 
          testLines (lines inputText) (0, 0)

testLines :: [String] -> (Int, Int) -> IO (Int, Int)
testLines [] ne = return ne
testLines (x:xs) (n, e) = do (putStrLn x)
                             userLine <- getLine
                             let new_n = (n + (length (words userLine)))
                             let new_e = (e + (errorCount x userLine))
                             putStrLn (show new_n ++ " words, " ++ show new_e ++ " errors")
                             testLines xs (new_n, new_e)

checkWord :: String -> String -> Int
checkWord a b
  | a == b    = 0
  | otherwise = 1

errorCount :: String -> String -> Int
errorCount a b = sum (zipWith checkWord (words a) (words b))
