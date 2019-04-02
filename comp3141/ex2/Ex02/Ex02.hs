module Ex02 where
import Test.QuickCheck
import Data.List
-- implement the following functions, which meet some (but not all!) of the 
-- properties of a correct sorting function

-- prop2 & 4, but not prop1 & 3 & 5
dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = xs


-- prop1 & 2 & 3, but not prop4 & 5
qsort [] = []
qsort (x:xs) = qsort small ++ mid ++ qsort large
      where
           small = [y | y<-xs, y<x]
           mid   = [y | y<-xs, y==x] ++ [x]
           large = [y | y<-xs, y>x]
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs
          
times :: [Int]-> Int -> [Int]
times x l 
      |l <= 0 = []
      |otherwise = x ++ (1): times x (l-1)
      
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = y
     where
          x :: [Int]
          x = rmdups xs
          y :: [Int]
          y = qsort x

-- prop1 & 3 & 4, but not prop2 & 5
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs =  times x (length xs)
     where 
         x = []

-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = hs
    where 
         x ::[Int]
         x = rmdups xs
         n :: Int
         n = (length xs) - (length x)
         ys = []
         y :: [Int]
         y = x ++ (times ys n)
         hs :: [Int]
         hs = qsort y


-- Properties of sorting function    
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = sortFn xs == sortFn (reverse xs)

sortProp2 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp2 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where 
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True
    
sortProp4 :: ([Int] -> [Int]) -> [Int] -> Bool    
sortProp4 sortFn xs = length xs == length (sortFn xs)

sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs 
  = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where 
    insertSorted x [] = [x]
    insertSorted x (y : ys) 
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys

