module Main where

import Data.Profunctor (lmap)
import Image

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Raster.Field
import Graphics.Gloss.Interface.IO.Game hiding (shift, circle)

data State = State { p :: Point, z :: Float }

main :: IO ()
main = do
  playField (InWindow "Mandelbrot" (640, 480) (100, 100)) (2, 2) 20 (State {p = (0,0), z= 1})
    (\s -> (frameImage (lmap (zoom (z s) . shift (p s)) mandelbrotImage)))
    handleEvents
    (\_ s -> s)

mandelbrotImage :: Image Point Color
mandelbrotImage = mandelbrot black (greyN . (mandelbrotCol 25)) 25


handleEvents :: Event -> State -> State
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) s  = s { p = (fst (p s) - 0.5*(z s), snd (p s))}
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) s = s { p = (fst (p s) + 0.5*(z s), snd (p s))}
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) s    = s { p = (fst (p s), snd (p s) + 0.5*(z s))}
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) s  = s { p = (fst (p s), snd (p s) - 0.5*(z s))}
handleEvents (EventKey (Char 'a') Down _ _) s = s { z = (z s) + (z s) * 0.1}
handleEvents (EventKey (Char 'o') Down _ _) s = s { z = (z s) - (z s) * 0.1}
handleEvents _ s = s


frameImage :: Image Point Color -> Point -> Color
frameImage img pt = runImage img pt

liveImage :: Image (Float, Point) Color -> Float -> Point -> Color
liveImage img d pt = (runImage img) (d, pt)


modPoint :: (Float -> Point -> Point) -> (Float, Point) -> Point
modPoint f (d, pt) = f d pt
