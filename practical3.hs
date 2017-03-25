import Test.LeanCheck
import Test.FitSpec

test_takeEmpty :: Bool
test_takeEmpty = length (take 5 []) == 0

test_takeTwo :: Bool
test_takeTwo = length (take 2 [1..5]) == 2

test_dropGreater :: Bool
test_dropGreater = length (drop 10 [1..5]) == 0

test_drop3of5 :: Bool
test_drop3of5 = length (drop 3 [1..5]) == 2

prop_takeFromEmpty :: Int -> Bool
prop_takeFromEmpty n = length (take n []) == 0

prop_takeMaxN :: Int -> [Int] -> Bool
prop_takeMaxN n xs = n >= 0 ==>
                     (length (take n xs)) <= n

prop_dropGreater :: Int -> [Int] -> Bool
prop_dropGreater n xs = n > length xs ==>
                        length (drop n xs) == 0
                        
prop_dropNofXs :: Int -> [Int] -> Bool
prop_dropNofXs n xs = n <= length xs && n >= 0 ==>
                      length (drop n xs) == (length xs) - n
                      
properties :: (Int -> [Int] -> [Int]) -> [Property]
properties take =
    [ property $ \n xs -> length (take n []) == 0
    ]

main = mainWith args {names = ["take n xs"]}
                take
                properties