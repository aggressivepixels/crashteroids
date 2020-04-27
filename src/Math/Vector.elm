module Math.Vector exposing (Vector, add, crossProduct, distance, from, lengthSquared, scale)


type alias Vector =
    ( Float, Float )


from : Float -> Float -> Vector
from =
    Tuple.pair


add : Vector -> Vector -> Vector
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


lengthSquared : Vector -> Float
lengthSquared ( x, y ) =
    (x ^ 2) + (y ^ 2)


scale : Float -> Vector -> Vector
scale by ( x, y ) =
    ( x * by, y * by )


distance : Vector -> Vector -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt (((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))


crossProduct : Vector -> Vector -> Float
crossProduct ( x1, y1 ) ( x2, y2 ) =
    (x1 * y2) - (x2 * y1)
