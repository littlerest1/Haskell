module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise
import Data.Bool

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.
extract :: Instructions->Instructions
extract (Move dst i) = i
extract (Turn deg i) = i
extract (SetStyle line i) = i
extract (SetColour colour i) = i
extract (PenUp i) = i
extract (PenDown i) = i

andThen :: Instructions -> Instructions -> Instructions
andThen i1 Stop = i1
andThen Stop i2 = i2
andThen (Move dst i) i2 = Move dst (andThen i i2)
andThen (Turn deg i) i2 = Turn deg (andThen i i2)
andThen (SetStyle line i) i2 = SetStyle line (andThen i i2)
andThen (SetColour colour i) i2 = SetColour colour (andThen i i2)
andThen (PenDown i) i2 = PenDown (andThen i i2)
andThen (PenUp i) i2 = PenUp (andThen i i2)              
          

loop :: Int -> Instructions -> Instructions
loop n i = 
     if n <= 0
        then 
            Stop
     else 
         f n i
         where 
             k :: Instructions
             k = i
             f :: Int -> Instructions -> Instructions
             f 1 i = i
             f n i = f (n-1) (andThen i k)

invisibly :: Instructions -> Instructions
invisibly Stop = PenDown Stop
invisibly (Move dst i) = PenUp (Move dst (invisibly i))
invisibly (Turn deg i) = PenUp (Turn deg (invisibly i))
invisibly (SetStyle line i) = PenUp (SetStyle line (invisibly i))
invisibly (SetColour colour i) = PenUp (SetColour colour (invisibly i))
invisibly (PenDown i) = invisibly i
invisibly (PenUp (PenUp i)) = 
           if i == Stop
              then PenUp (PenUp Stop)
           else invisibly (PenUp i)
invisibly i = 
     if both i x y == 1
        then i
     else 
         invisibly (f i)
     where 
          x :: Int
          x = 0
          y :: Int
          y =0
          both :: Instructions -> Int ->Int ->Int
          both  Stop x y= x + y
          both (PenUp i) x y = both i (x+1) y
          both (PenDown i) x y = both i x (y+1)
          both i x y = both (extract i) x y
          
          f :: Instructions -> Instructions
          f (PenUp Stop) = PenUp Stop
          f (PenUp i) = f i
          f (PenDown i) = f i 
          f i = i

rotate :: Angle -> Angle -> Angle
rotate r a = revolve (a + r)
   where
     revolve n | n >   180 = revolve (n - 360)
               | n <= -180 = revolve (n + 360)
               | otherwise = n
 
  
finalState' :: Instructions -> TortoiseState -> TortoiseState
finalState' i something = snd (tortoise i something)
     
     
retrace :: Instructions -> Instructions
retrace Stop = Stop
--retrace (PenUp Stop) = PenDown Stop
--retrace (Turn deg Stop) = Turn (negate deg) Stop
retrace i = xn (f i setC setS setP save) reverse
        where
           setC :: Instructions
           setC = SetColour white Stop
           setS :: Instructions
           setS = SetStyle (Solid 1) Stop
           setP :: Instructions
           setP = PenDown Stop
           save :: [Instructions]
           save = []
           
           f :: Instructions -> Instructions-> Instructions-> Instructions -> [Instructions] ->[Instructions]
           f Stop setC setS setP save = save ++ [(SetColour white(SetStyle (Solid 1) (PenDown Stop)))]
           f (Move dst i) setC setS setP save = f i setC setS setP ([setC `andThen` setS `andThen` setP `andThen` (Move (-dst) Stop)] ++ save)
           f (Turn deg i) setC setS setP save = f i setC setS setP ([Turn (negate deg) Stop] ++ save)
           f (SetColour colour i) setC setS setP save = f i (SetColour colour Stop) setS setP save
           f (SetStyle line i) setC setS setP save = f i setC (SetStyle line Stop) setP save
           f (PenDown i) setC setS setP save = f i setC setS (PenDown Stop) save
           f (PenUp i) setC setS setP save = f i setC setS (PenUp Stop) save                 
                              
           reverse :: Instructions
           reverse = Stop
           xn :: [Instructions] -> Instructions -> Instructions
           xn [] reverse = reverse
           xn (x:xs) reverse = xn xs (reverse `andThen` x)
                
                
overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay (y:ys) = (y `andThen` (invisibly(retrace y))) `andThen` (overlay ys)

