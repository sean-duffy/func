import System.Environment

main :: IO ()
main = intro

intro :: IO ()
intro  =  do putStr "What is your name? "
             n <- getLine
             putStrLn ("Hello, "++ n ++"!")
             p  <- getProgName
             putStrLn ("My name is "++ p ++".")
