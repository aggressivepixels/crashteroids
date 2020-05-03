module Basics.Extra exposing (flip, fmodBy)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


fmodBy : Float -> Float -> Float
fmodBy modulus value =
    value - modulus * toFloat (floor (value / modulus))
