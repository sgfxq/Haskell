module Geometry
(
 Point,
 distance,
 distanceBetween,
 getX,
 getY,
 Line,
 Shape,
 Vector
) where

data Point = Origin | Point Float Float deriving (Show)
data Line = Line Point Point deriving (Show)
data Shape = Rect Point Point | Circle Point Float deriving (Show)
data Vector = Vector Float Float deriving (Show)


distance :: Point -> Float
distance Origin = 0
distance (Point x y) = sqrt (x*x+y*y)

getX :: Point -> Float
getX Origin = 0
getX (Point x y) = x

getY :: Point -> Float
getY Origin = 0
getY (Point x y) = y

distanceBetween :: Point -> Point -> Float
distanceBetween p1 p2 = sqrt (((getX p1)-(getX p2))^2 + ((getY p1)-(getY p2))^2)

lineLen :: Line -> Float
lineLen (Line p1 p2) = distanceBetween p1 p2

area :: Shape -> Float
area (Rect p1 p2) =  ((getX p2)-(getX p1))*((getY p2)-(getY p1))
area (Circle _  r) = pi * r * r

translatePoint :: Vector -> Point -> Point
translatePoint (Vector x y) p = Point (x + getX p) (y + getY p)

translateShape :: Vector -> Shape -> Shape
translateShape v (Rect p1 p2) = Rect (translatePoint v p1) (translatePoint v p2)
translateShape v (Circle p r) = Circle (translatePoint v p) r

containsPoint :: Shape -> Point -> Bool
containsPoint (Rect (Point x1 y1) (Point x2 y2)) (Point x y) = ( (x-x1)*(x-x2) <= 0 ) && ((y-y1)*(y-y2) <= 0) 
containsPoint (Circle (Point rx ry) r) (Point x y) = ((abs $ rx - x) <= r) && ((abs $ ry -y) <= r)
