module Ex03 where

import Test.QuickCheck
import Data.List(sort, nub)


data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) 
  = allTree (< v) l  && 
    allTree (>= v) r &&
    isBST l          && 
    isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True
        
--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r) 
  | i < v     = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

----------------------

mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred i Leaf = False
mysteryPred i (Branch t l r)
   | i == t = True
   | i < t =  mysteryPred i l
   | i > t = mysteryPred i r


prop_mysteryPred_1 integer = 
  forAll searchTrees $ \tree -> mysteryPred integer (insert integer tree)

prop_mysteryPred_2 integer = 
  forAll searchTrees $ \tree -> not (mysteryPred integer (deleteAll integer tree))
----------------------
qsort [] = []
qsort (x:xs) = qsort small ++ mid ++ qsort large
      where
           small = [y | y<-xs, y<x]
           mid   = [y | y<-xs, y==x] ++ [x]
           large = [y | y<-xs, y>x]

mysterious :: BinaryTree -> [Integer]
mysterious t = xs
    where
         x :: [Integer]
         x = []
         y = addI t x
         addI :: BinaryTree ->[Integer]-> [Integer]
         addI Leaf x = []
         addI (Branch t l r) x = x ++ (t) : (addI l x ++ addI r x)
         xs :: [Integer]
         xs = qsort y
         


isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

prop_mysterious_1 integer = forAll searchTrees $ \tree -> 
  mysteryPred integer tree == (integer `elem` mysterious tree)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious
----------------------


-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

depth Leaf = 0
depth (Branch _ t u) = (max (depth t) (depth u)) + 1

balFactor :: BinaryTree -> BinaryTree -> Int
balFactor t u = (depth t) - (depth u)

balanceLL (Branch v (Branch vl tl ul) u)  = (Branch vl tl (Branch v ul u))
balanceLR (Branch v (Branch vl tl (Branch vlr tlr ulr)) u) = (Branch vlr (Branch vl tl tlr) (Branch v ulr u))
balanceRL (Branch v t (Branch vr (Branch vrl trl url) ur)) = (Branch vrl (Branch v t trl) (Branch vr url ur)) 
balanceRR (Branch v t (Branch vr tr ur)) = (Branch vr (Branch v t tr) ur)


left (Branch _ t _) = t
right (Branch _ _ u) = u
value (Branch v _ _) = v

insertT :: BinaryTree -> Integer -> BinaryTree
insertT Leaf i = (Branch i Leaf Leaf)
insertT (Branch v t u) i
    | i == v = (Branch v t u)
    | i < v && (balFactor ti u) ==  2 && i < value t = balanceLL (Branch v ti u)
    | i < v && (balFactor ti u) ==  2 && i > value t = balanceLR (Branch v ti u)
    | i > v && (balFactor t ui) == -2 && i < value u = balanceRL (Branch v t ui)
    | i > v && (balFactor t ui) == -2 && i > value u = balanceRR (Branch v t ui)
    | i < v  = (Branch v ti u)
    | i > v  = (Branch v t ui)
        where ti = insertT t i
              ui = insertT u i
  
astonishing :: [Integer] -> BinaryTree
astonishing xs = bst xs n y
      where 
           y :: BinaryTree
           y = Leaf
           n :: Int
           n = length xs
           bst ::[Integer] -> Int -> BinaryTree -> BinaryTree
           bst []n y = y
           bst (x:xs) n y  
               | n == 0 = y
               | otherwise = bst xs (n-1) (insertT y x)
               
               
prop_astonishing_1 
  = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2 
  = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3 
  = forAll sortedListsWithoutDuplicates $ \ integers -> 
    mysterious (astonishing integers) == integers


isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = and [ abs (height l - height r) <= 1 
                                , isBalanced l 
                                , isBalanced r
                                ]
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)

