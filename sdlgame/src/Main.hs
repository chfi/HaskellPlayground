{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import qualified Data.Vector.Storable.Mutable as Vec

import Control.Arrow
import Control.Category
import qualified Control.Monad as CM
import Debug.Trace as Debug
import Foreign.C.Types
import Linear
import Linear.Affine
import Linear.Metric as LM
import Linear.V2 (angle)
import SDL (($=))
import qualified SDL
import qualified SDL.Input.Keyboard.Codes as Key
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Wire
import FRP.Netwire


import System.IO
import Prelude hiding ((.), id)


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


playerRadius = 5


type PlayerHead = V2 Double
type PlayerTail = [V2 Double]

-- pTail should be a NonEmpty (V2 Double)
data Player = Player { pHead :: V2 Double
                     , pTail :: [V2 Double]
                     , pDist :: Double
                     , pAlive :: Bool
                     -- , pAngle :: Double -- Current player angle in radians
                     -- , pSpeed :: Double
                     }


-- Ideally, this should be framerate independent,
-- that is, it should place however many points into the tail
-- that fit between the current head of the tail and the player head
playerExtendTail :: Player -> Player
playerExtendTail p@Player{..} =
  let d = LM.distance (head pTail) pHead
      pT' = if d > pDist then pHead : pTail else pTail
  in p { pTail = pT' }


-- removes the first parts of the tail that are "colliding" with the head
-- returns a tail that can be collided with by the player, without false positives
playerSafeTail :: Player -> PlayerTail
playerSafeTail Player{..} =
  dropWhile (\th -> LM.distance pHead th < playerRadius*2) pTail

playerCheckCollision :: Player -> PlayerTail -> Bool
playerCheckCollision Player{..} p2t =
  any (\th -> LM.distance pHead th < playerRadius*2) p2t


selfColWire :: HasTime t s => Wire s () IO Player Bool
selfColWire = proc p -> do
  let tl = playerSafeTail p
      val = playerCheckCollision p tl
  returnA -< val

plWire :: HasTime t s => Player -> Wire s () IO (Set SDL.Keysym) Player
plWire p = proc keys -> do
  pH <- plPosWire (pHead p) -< keys
  rec pT <- delay (pTail p) -< pT'
      let p' = playerExtendTail p { pHead = pH, pTail = pT }
          pT' = pTail p'
  alive <- (arr not) . selfColWire -< p'
  returnA -< p' { pAlive = alive }


plPosWire :: HasTime t s => (V2 Double) -> Wire s () IO (Set SDL.Keysym) (V2 Double)
plPosWire init = proc keys -> do
  pos <- integral init . (angvel >>> polarVel) -< keys
  returnA -< pos

angWire :: HasTime t s => Double -> Wire s () IO (Set SDL.Keysym) Double
angWire da = proc keys -> do
  -- pretty ugly --- need to create constant arrows to feed the speed, since it's a double.
  va <-  (arr (const (-da))) . when (isKeyDown Key.ScancodeA)
     <|> (arr (const   da ))   . when (isKeyDown Key.ScancodeD)
     <|> 0
          -< keys

  integral 0 -< va

-- wire that takes an angle and returns a cartesian vector
polarVelWire :: HasTime t s => Double -> Wire s () IO Double (V2 Double)
polarVelWire speed = proc ang -> do
  v <- arr angle -< ang
  returnA -< (v * (V2 speed speed))


vel
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) (V2 Double)
vel = proc keys -> do
  vx <-  (-100) . when (isKeyDown Key.ScancodeA)
     <|>   100  . when (isKeyDown Key.ScancodeD)
     <|> 0
          -< keys
  vy <-  (-100) . when (isKeyDown Key.ScancodeW)
     <|>   100  . when (isKeyDown Key.ScancodeS)
     <|> 0
          -< keys
  returnA -< V2 vx vy


angvel :: HasTime t s => Wire s () IO (Set SDL.Keysym) Double
angvel = proc keys -> do
  va <-  (-3) . when (isKeyDown Key.ScancodeA)
     <|>   3  . when (isKeyDown Key.ScancodeD)
     <|> 0
          -< keys
  va' <- integral 0 -< va
  returnA -< va'

-- wire that takes an angle and returns a cartesian vector
-- i suppose this should return a normalized movement vector
polarVel :: HasTime t s => Wire s () IO Double (V2 Double)
polarVel = proc ang -> do
  v <- arr angle -< ang
  returnA -< (v * 100)



vel'
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) (V2 Double)
vel' = liftA (uncurry V2) $ velX &&& velY


velX
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) Double
velX =
        (-200) . when (isKeyDown Key.ScancodeA)
    <|> 200 . when (isKeyDown Key.ScancodeD)
    <|> 0


velY
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) Double
velY =  pure (-200) . when (isKeyDown Key.ScancodeW)
    <|> pure   200  . when (isKeyDown Key.ScancodeS)
    <|> pure 0



-- Wire that fetches SDL keyboard events & outputs a set containing all pressed keys
inputWire :: Wire s e IO (Set SDL.Keysym) (Set SDL.Keysym)
inputWire = mkGen_ $ fmap Right . getEvents


-- SDL event helper functions
parseEvents :: Set SDL.Keysym -> [SDL.Event] -> Set SDL.Keysym
parseEvents =
    foldr
        (\ev s ->
              case SDL.eventPayload ev of
                  SDL.KeyboardEvent (SDL.KeyboardEventData _ m _ ks) ->
                      case m of
                          SDL.Pressed -> Set.insert ks s
                          SDL.Released -> Set.delete ks s
                  _ -> s)

getEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
getEvents oldKeys = parseEvents oldKeys <$> SDL.pollEvents


isKeyDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
isKeyDown sc = not . Set.null . Set.filter ((== sc) . SDL.keysymScancode)



circleTexture :: SDL.Renderer -> IO SDL.Texture
circleTexture r = do
  t <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (V2 20 20)
  SDL.rendererRenderTarget r $= Just t

  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r
  let w = 20 :: CInt
      h = 20 :: CInt
      p = 4 :: CInt
      inCircle :: V2 CInt -> Bool
      inCircle p = LM.distance (fmap fromIntegral p) (V2 10 10) < 10
      points = [(x,y) | x <- [0..19], y <- [0..19], inCircle (V2 x y)]

  SDL.rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
  sequence_ $ fmap (\(x,y) -> SDL.drawPoint r (P $ V2 x y)) points

  SDL.rendererRenderTarget r $= Nothing
  SDL.textureBlendMode t $= SDL.BlendAlphaBlend
  return t


drawPlayer :: SDL.Renderer -> Player -> IO ()
drawPlayer r Player{..} = do
  t <- circleTexture r
  let hd = Just $ SDL.Rectangle (P $ fmap round pHead) (V2 10 10)
      tl = fmap (\p -> Just $ SDL.Rectangle (P $ fmap round p) (V2 10 10)) pTail
      draw' x = SDL.copy r t Nothing x
  draw' hd
  sequence_ (fmap draw' tl)
  return ()


circleSurf :: IO SDL.Surface
circleSurf = do
  let w = 20 :: CInt
      h = 20 :: CInt
      p = 4 :: CInt
      inCircle x y = (x*x + y*y) < 25
  vec <- Vec.new (fromIntegral (w*h*w*p))
  Vec.set vec 0x00
  let points = [(x,y) | x <- [0..19], y <- [0..19], inCircle x y]
      drawP i v = sequence_ $ fmap (\x -> Vec.write v x 0xFF) i
      is = [[i, i+1, i+2, i+3] | i <- [0..20*20], i `mod` 5 == 0]
  sequence_ $ fmap (\x -> drawP x vec) is
  SDL.createRGBSurfaceFrom vec (V2 w h) (w*p) SDL.RGBA8888

-- the player drawing function should really fill in teh whole area between the pieces...
-- if I had a function that does so between two points, I could fold over the tail, with
-- the head as initial value.


-- Main loop
runGameWire
    :: Set SDL.Keysym
    -> SDL.Renderer
    -> Session IO s
    -> Wire s e IO (Set SDL.Keysym) Player
    -> IO b
runGameWire keys r s w = do
    SDL.rendererDrawColor r $= V4 0 0 0 0
    SDL.clear r
    (ds,s') <- stepSession s
    (Right ev,_) <- stepWire inputWire ds $ Right keys
    (Right p',w') <- stepWire w ds $ Right ev
    CM.when (playerCheckCollision p' (playerSafeTail p')) (putStrLn "Player colliding")
    SDL.rendererDrawColor r $= V4 0 0 maxBound maxBound
    drawPlayer r p'
    SDL.present r
    -- SDL.delay (1000 `div` 10)
    runGameWire ev r s' w'


player :: Player
player = Player { pHead = (V2 300 300)
                , pTail = [(V2 300 300)]
                , pDist = 1
                , pAlive = True
                }


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality
       CM.when (renderQuality /= SDL.ScaleLinear) $
           putStrLn "Warning: Linear texture filtering not enabled!"
    window <-
        SDL.createWindow
            "SDL Tutorial"
            SDL.defaultWindow
            { SDL.windowInitialSize = V2 screenWidth screenHeight
            }
    SDL.showWindow window
    renderer <-
        SDL.createRenderer
            window
            (-1)
            SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedVSyncRenderer
            , SDL.rendererTargetTexture = False
            }
    _ <- runGameWire Set.empty renderer clockSession_ $ plWire player
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
