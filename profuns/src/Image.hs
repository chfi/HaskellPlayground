{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Image where

import Data.Complex
import Data.Monoid (Monoid())
import Data.Profunctor

type Coord = (Float, Float)

newtype Image a b = Image { runImage :: (a -> b) }
                  deriving (Profunctor, Monoid)

hypot :: Coord -> Float
hypot (x,y) = sqrt (x*x + y*y)

inCircle :: Float -> Coord -> Bool
inCircle r c = hypot c <= r


shift :: Coord -> Coord -> Coord
shift (x, y) (x', y') = (x+x', y+y')

zoom :: Float -> Coord -> Coord
zoom a (x, y) = (a*x, a*y)

rot :: Float -> Coord -> Coord
rot a (x, y) = ( x * cos a - y * sin a
               , x * sin a + y * cos a)

rotAt :: Coord -> Float -> Coord -> Coord
rotAt (x,y) a = shift (-x,-y) . rot a . shift (x,y)


leftAndRight :: color -> color -> Image Coord color
leftAndRight l r = Image $ \(x, _) -> if x < 0 then l else r

topAndBottom :: color -> color -> Image Coord color
topAndBottom t b = Image $ \(_, y) -> if y < 0 then b else t

circle :: Float -> color -> color -> Image Coord color
circle r i o = Image $ \c -> if inCircle r c then i else o

mandelbrot :: color -> (Int -> color) -> Int -> Image Coord color
mandelbrot divCol f l = Image $ \(x, y) ->
    let ms = iterate (\z -> z^2 + (x :+ y)) 0
        n = length $ take l $ takeWhile ((<2) . magnitude) ms
    in if n == l then divCol else f n

mandelbrotCol :: Int -> Int -> Float
mandelbrotCol m n = 1 - (fromIntegral n / fromIntegral m)
