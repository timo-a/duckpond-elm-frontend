module Math exposing (..)

tau : Float
tau = 2 * pi

type alias PolarCoordinate = (Float, Float)

toPolar : (Float, Float) -> PolarCoordinate
toPolar (x, y) = (sqrt ( x^2 + y^2 ), atan2 y x)

toCartesian : PolarCoordinate -> (Float, Float)
toCartesian (r, theta) = (r * (cos theta), r * (sin theta))

add: (Float, Float) -> (Float, Float) -> (Float, Float)
add (x, y) (x2, y2) = (x + x2, y + y2)
subtract: (Float, Float) -> (Float, Float) -> (Float, Float)
subtract (x, y) (x2, y2) = (x - x2, y - y2)

subtractPolar: (Float, Float) -> (Float, Float) -> (Float, Float)
subtractPolar a b = toPolar <| on subtract toCartesian a b

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g = \a b -> f (g a) (g b)

fModBy : Float -> Float -> Float
fModBy m a =  a - m * ( (ffloor) ( a / m ))

ffloor : Float -> Float
ffloor = Basics.toFloat << Basics.floor