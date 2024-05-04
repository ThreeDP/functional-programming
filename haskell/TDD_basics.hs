import Data.List (partition)

assertEqual :: Eq a => a -> a -> IO ()
assertEqual a b = if a == b then putStr "." else putStr "X"

assertEqualLst :: Eq a => [a] -> [a] -> IO ()
assertEqualLst [] [] = putStr "."
assertEqualLst [] _ = putStr "X"
assertEqualLst _ [] = putStr "X"
assertEqualLst (x:xs) (y:ys) =  if x == y
                                then assertEqualLst xs ys
                                else putStr "X"

main :: IO ()
main = do
-- testContainsFirstLst
  putStrLn $ "Test Contains First List"
  assertEqual (containsFirstLst [1, 2, 3, 4, 5] [1, 2, 3, 4]) False
  assertEqual (containsFirstLst [1, 2, 3] [1, 2, 3, 4]) True
  assertEqual (containsFirstLst [] [1, 2, 3, 4]) True
  assertEqual (containsFirstLst [1, 2, 3] []) False
  putStrLn $ "\n"
  
-- testListOddsPos
  putStrLn $ "Test List Odds Positions"
  assertEqualLst (lstOddsPos [1, 2, 3, 4, 5, 6, 7]) [2, 4, 6]
  assertEqualLst (lstOddsPos []) []
  assertEqualLst (lstOddsPos [1]) []
  assertEqualLst (lstOddsPos [0, 1]) [1]
  putStrLn $ "\n"

-- testBinary
  putStrLn $ "Test Convert decimal to binary"
  assertEqualLst (binary 10) [1,0,1,0]
  assertEqualLst (binary 1) [1]
  assertEqualLst (binary 0) [0]
  assertEqualLst (binary 3) [1, 1]
  putStrLn $ "\n"
                   
containsFirstLst :: (Ord a, Num a) => [a] -> [a] -> Bool
containsFirstLst _ [] = False
containsFirstLst [] _ = True
containsFirstLst (x:xs) (y:ys) = if x == y
                              then containsFirstLst xs ys
                              else False
                              
binary :: (Integral a, Ord a, Num a) => a -> [a]
binary 0 = [0]
binary 1 = [1]
binary n = binary (n `div` 2) ++ [(n `mod` 2)]

lstOddsPos :: (Ord a, Num a) => [a] -> [a]
lstOddsPos [] = []
lstOddsPos [x] = []
lstOddsPos (even:odd:xs) = odd : lstOddsPos xs

sufix :: (Ord a, Num a) => [a] -> [[a]]
sufix [] = [[]]
sufix xs = xs : sufix (ftTail xs)
