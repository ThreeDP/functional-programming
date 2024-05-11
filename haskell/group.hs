import Data.List (partition)
import Basics

main :: IO ()
main = do
  let unsorted = [1..700]
  putStrLn $ show $ comp unsorted (\x -> x `mod` 2 == 0)
  putStrLn $ show $ comp unsorted ftEven
  putStrLn $ show $ comp unsorted ftOdd
                   
comp :: (Ord a, Eq a, Num a) => [a] -> (a -> Bool) -> [a]
comp [] f = []
comp (x:xs) f = if f x
                then x : comp xs f
                else comp xs f
                
ftEven :: (Integral n) => n -> Bool
ftEven n = if mod n 2 == 0 then True else False

ftOdd :: Int -> Bool
ftOdd n = if mod n 2 /= 0 then True else False