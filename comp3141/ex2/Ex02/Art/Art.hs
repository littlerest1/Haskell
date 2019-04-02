module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = fracTree 15 150 10 -- replace with something else

fracTree :: Float -> Float -> Int -> Picture
fracTree width height n
  = fTree (Point  (200 - width / 2) 800) (Vector 30 (-height))
                  (Vector width 0) green n
  where

    angle = pi/12
    fTree :: Point -> Vector -> Vector -> Colour -> Int -> Picture
    fTree pos vec1 vec2 col n
      | n < 1 = [Path [pos, movePoint vec1 pos, 
                                movePoint vec2 $ movePoint vec1 pos, 
                                movePoint vec2 pos]
                          col
                          Solid]
                          
      | otherwise = fTree pos vec1 vec2 col (n - 1) ++
                    fTree (movePoint vec1 pos) 
                          (scaleVector 1.8 $ rotateVector (1.8 * angle) vec1)
                          (scaleVector 1.8 $ rotateVector (1.8 * angle) vec2) 
                          (green) (n - 1) ++
                    fTree (movePoint vec1 pos) 
                          (scaleVector 0.2 $ rotateVector (-angle) vec1)
                          (scaleVector 0.2 $ rotateVector (-angle) vec2) 
                          (green) (n - 1) 

      

scaleVector :: Float -> Vector -> Vector
scaleVector fac (Vector x y)
  = Vector (fac * x) (fac * y)                           
 
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
  = Point (xv + xp) (yv + yp)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
