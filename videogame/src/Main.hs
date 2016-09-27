{-# LANGUAGE RecordWildCards #-}

module Main where


import qualified Control.Wire as W
import Prelude hiding ((.), id)


import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.Pure.Game (Event)



-- wire :: (Monad m) => Wire s () m a String
-- wire = pure 15


-- data Point = Point (Float, Float) deriving (Eq, Show)

data GameState = GameState { _wire :: W.SimpleWire Float World
                           }

-- data GameState = GameState { world :: World
--                            , _wire :: W.SimpleWire Float World
--                            }

data World = World { x :: Float
                   , y :: Float
                   , dx :: Float
                   , dy :: Float
                   } deriving (Eq, Show)

initWorld = World 0 0 1.0 1.0


-- step' :: Float ->
step' = 


step :: Float -> World -> World
step d w = w { x = x w + dx w
             , y = y w + dy w
             }

draw :: World -> Picture
draw World{..} = translate x y $ circle 5


ev :: Event -> World -> World
ev e w@(World{..}) = case e of
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { dx = -1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { dx = 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { dy = 1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { dy = -1 }
  EventKey (SpecialKey KeyLeft) Up _ _ -> w { dx = 0 }
  EventKey (SpecialKey KeyRight) Up _ _ -> w { dx = 0}
  EventKey (SpecialKey KeyUp) Up _ _ -> w { dy = 0 }
  EventKey (SpecialKey KeyDown) Up _ _ -> w { dy = 0}
  _ -> w


main :: IO ()
-- play ::
--   Display
--   -> Color
--   -> Int
--   -> world
--   -> (world -> Picture)
--   -> (Graphics.Gloss.Interface.Pure.Game.Event -> world -> world)
--   -> (Float -> world -> world)
--   -> IO ()
main = play (InWindow "Video game" (640, 480) (10, 10))
            white
            100
            initWorld
            draw
            -- (\str -> Translate (-300) 0 $ Scale 0.1 0.1 $ Text str)
            -- (\event _ -> show event)
            ev
            -- (\_ w -> w)
            step
            -- (\d world -> world)
