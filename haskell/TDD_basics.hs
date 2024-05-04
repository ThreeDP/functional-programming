import Data.List (partition)
import Basics

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

-- testftFatLst
  putStrLn $ "Test Fatorial of a list"
  assertEqualLst (ftFatLst [1, 2, 3, 4, 5]) [1, 2, 6, 24, 120]
  assertEqualLst (ftFatLst []) []
  assertEqualLst (ftFatLst [0]) [1]
  putStrLn $ "\n"

-- testListEvens
  putStrLn $ "Test List Evens - return only the even numbers of a list"
  assertEqualLst (ftLstEvens [1, 2, 3, 4, 5, 6, 27]) [2, 4, 6]
  assertEqualLst (ftLstEvens []) []
  assertEqualLst (ftLstEvens [0, 1]) [0]
  assertEqualLst (ftLstEvens [1]) []
  putStrLn $ "\n"

-- testInterleaveList
  putStrLn $ "Test interleave a list"
  assertEqualLst (ftInterleave [1, 2] [3, 4]) [1, 3, 2, 4]
  assertEqualLst (ftInterleave [] []) []
  assertEqualLst (ftInterleave [1] []) [1]
  assertEqualLst (ftInterleave [] [1]) [1]
  putStrLn $ "\n"

-- testFindEven
  putStrLn $ "Test find even and return true if has at least one even number"
  assertEqual (ftFindEven [1, 2, 3, 4, 5]) True
  assertEqual (ftFindEven []) False
  assertEqual (ftFindEven [1, 3, 5, 7, 9]) False
  assertEqual (ftFindEven [1, 2, 3]) True
  putStrLn $ "\n"

-- testOnlyOdds
  putStrLn $ "Test if the list has only odds numbers"
  assertEqual (ftOnlyOdds [1, 2, 3, 4, 5]) False
  assertEqual (ftOnlyOdds []) False
  assertEqual (ftOnlyOdds [1, 3, 5, 7, 9]) True
  assertEqual (ftOnlyOdds [1, 2, 3]) False
  putStrLn $ "\n"

-- Functions 
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

ftFatLst :: (Integral a, Eq a) => [a] -> [a]
ftFatLst [] = []
ftFatLst (x:xs) = ftFat x : ftFatLst xs

ftLstEvens :: (Integral a, Ord a) => [a] -> [a]
ftLstEvens [] = []
ftLstEvens (x:xs) =  if x `mod` 2 == 0
                    then x : ftLstEvens xs
                    else ftLstEvens xs

ftInterleave :: (Ord a, Eq a, Num a) => [a] -> [a] -> [a]
ftInterleave [] [] = []
ftInterleave xs [] = xs
ftInterleave [] ys = ys
ftInterleave (x:xs) (y:ys) = x : y : ftInterleave xs ys

ftFindEven :: (Integral a, Eq a, Num a) => [a] -> Bool
ftFindEven [] = False
ftFindEven (x:xs) = if x `mod` 2 == 0
                  then True
                  else ftFindEven xs

ftOnlyOdds :: (Integral a, Eq a, Num a) => [a] -> Bool
ftOnlyOdds [] = False
ftOnlyOdds [x] = if x `mod` 2 /= 0 then True else False
ftOnlyOdds (x:xs) = if x `mod` 2 /= 0
                    then ftOnlyOdds xs
                    else False

sufix :: (Ord a, Num a) => [a] -> [[a]]
sufix [] = [[]]
sufix xs = xs : sufix (ftTail xs)
