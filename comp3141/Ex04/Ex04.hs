module Ex04 where

import Text.Read (readMaybe)
import Data.Maybe
import Data.List

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise str = mapM parseToken(words(str))

newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
pop = C f
    where
         f [] = Nothing
         f (x:xs) = Just (xs,x)

push :: Int -> Calc ()
push i = C f 
     where 
        f xs = Just ([i]++xs,())
         
instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a
    
addTwo :: Calc ()
addTwo = do
        x <- pop
        y <- pop
        push (x + y)
        
minus :: Calc()
minus = do
     x <- pop
     y <- pop
     push (x - y)

times :: Calc()
times = do
      x <- pop
      y <- pop
      push(x * y)
      
divide :: Calc()
divide = do
      x <- pop
      y <- pop
      push(x `div` y)
      
evaluate :: [Token] -> Calc Int
evaluate [] = pop
evaluate (Number t: ts) = do 
                          push t
                          evaluate ts
                          
evaluate(Operator t : ts) = do
                        x <- pop
                        y <- pop
                        push (t x y)
                        evaluate ts         


calculate :: String -> Maybe Int
calculate s = cs
     where          
          runCalc (C f) = f
          x = tokenise s     
          y = evaluate (fromJust x)
          cs = runCal y
          
          
