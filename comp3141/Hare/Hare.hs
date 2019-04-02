{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char 
  Seq  :: RE a -> RE b -> RE (a,b)
  Choose :: RE a -> RE a -> RE a
  Star  :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b
  
match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char str) = do
     x <- readCharacter
     guard (x `elem` str) 
     pure x
match (Choose a b) = match a <|> match b
match (Seq a b) = do
      ra <- match a
      rb <- match b
      pure(ra,rb)

match (Action re a) = fmap re (match a)
match (Star a) =
     addFront <$> match a <*> match (Star a)
    <|> pure []
  where 
    addFront x xs = (x:xs)
--    addFront _ _ = error "(should be) impossible!"

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)


infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action addFront (Seq x xs)
     where
         addFront (x,xs) = (x:xs)
                 
string :: String -> RE String
string [] = Action (const []) Empty
string (x:xs) = cons (Char [x]) (string xs)

rpt :: Int -> RE a -> RE [a]
rpt 0 re = Star Fail
rpt n re = Action addFront (Seq re (rpt (n-1) re))
   where
      addFront (x,xs) = (x:xs)


rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re
           | x == y = rpt x re
           | otherwise = Choose (rpt y re) (rptRange (x,(y-1)) re)
   --  where
       -- addFront (x,xs) = (x:xs)
-- (Choose (rpt y re) (rpt x re)) 

option :: RE a -> RE (Maybe a)
--option o = Action Just (Choose o Fail)
option o = Choose (Action Just (Choose o Fail)) (Action (const Nothing) Empty)

plus :: RE a -> RE [a]
plus re = Action addFront (Seq re (Star re))
    where
         addFront (x,xs) = (x:xs)

choose :: [RE a] -> RE a
choose [] = Fail
choose (x:xs) = Choose x (choose xs)

