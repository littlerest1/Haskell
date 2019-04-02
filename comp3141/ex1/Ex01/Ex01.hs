module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house ::PictureObject
    house = Path{pointsPO =houseList,colourPO = green,lineStylePO = Solid}
    door :: PictureObject
    door  = Path{pointsPO = doorList,colourPO = red,lineStylePO = Solid}
-- these are the coordinates - convert them to a list of Point
houseCOs ::[(Float,Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),(730, 450), (700, 450), (700, 750)]

mkpoint :: (Float,Float)->Point
mkpoint    (x,y) = (Point x y)

myMap :: (a->b) -> [a]-> [b]
myMap fn [] 
  = []
myMap fn  (x:xs) 
  = fn x  : myMap fn xs

houseList :: [Point]
houseList = myMap(mkpoint)houseCOs

doorCOs :: [(Float,Float)]
doorCOs =[(420, 750), (420, 550), (580, 550), (580, 750)]

doorList :: [Point]
doorList = myMap(mkpoint)doorCOs

----doing chinmey house containing house door smoke and chinmey----
grey :: Colour
grey = Colour 255 255 255 128

smokeCOs ::[(Float,Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

smokeList :: [Point]
smokeList = myMap(mkpoint)smokeCOs

chimneyCOs ::[(Float,Float)]
chimneyCOs = [(300,750),(300,450),(270,450),(500,200),(615,325),(615,250),(650,250),(650,363),(730,450),(700,450),(700,750)]

chimneyList :: [Point]
chimneyList = myMap(mkpoint)chimneyCOs

smoke :: PictureObject
smoke = Path{pointsPO = smokeList,colourPO = grey,lineStylePO = Solid}
chimney :: PictureObject
chimney = Path{pointsPO = chimneyList,colourPO = green,lineStylePO = Solid}
door :: PictureObject
door  = Path{pointsPO = doorList,colourPO = red,lineStylePO = Solid}
chimneyHouse :: Picture
chimneyHouse = [chimney,smoke,door]



-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)
 
movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) = Path after colour lineStyle
   where 
        after :: [Point]
        after = myMap(moving)points
        moving :: Point -> Point
        moving (Point x y) = movePoint (Point x y) vec
movePictureObject vec (Polygon points colour lineStyle fillStyle) = Polygon after colour lineStyle fillStyle
   where 
        after :: [Point]
        after = myMap(moving)points
        moving :: Point -> Point
        moving (Point x y) = movePoint (Point x y) vec
movePictureObject vec (Circle center radius colour lineStyle fillStyle) = Circle after radius colour lineStyle fillStyle
    where 
        after :: Point
        after =  movePoint center vec
movePictureObject vec (Ellipse center width height rotation colour lineStyle fillStyle) = Ellipse after  width height rotation colour lineStyle fillStyle
    where 
        after :: Point
        after =  movePoint center vec
            


-- add other cases



-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]

simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = times oriCircles n
    where     
         start :: Float
         start = n
         oriCircles :: Picture
         oriCircles = []
         times :: Picture ->Float -> Picture
         times oriCircles n
               | n <= 0 = []
               |otherwise = 
                oriCircles ++ (Circle{centerPO = (Point 400 400),radiusPO = (n *400/start),colourPO = col,lineStylePO = Solid,fillStylePO = SolidFill} ): times oriCircles (n-1)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)
