{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Control.Arrow
import Control.Category
import qualified Control.Monad as CM
import Debug.Trace as Debug
import Foreign.C.Types
import Linear
import Linear.Affine
import Linear.Metric as LM
import SDL (($=))
import qualified SDL
import qualified SDL.Input.Keyboard.Codes as Key
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Wire
import FRP.Netwire

import Prelude hiding ((.), id)


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

-- pTail should be a NonEmpty (V2 Double)
data Player = Player { pHead :: V2 Double
                     , pTail :: [V2 Double]
                     , pDist :: Double
                     }


playerExtendTail :: Player -> Player
playerExtendTail p@Player{..} =
  let d = LM.distance (head pTail) pHead
      pT' = if d > pDist then pHead : pTail else pTail
  in p { pTail = pT' }


plWire :: HasTime t s => Player -> Wire s () IO (Set SDL.Keysym) Player
plWire p = proc keys -> do
  pH <- integral (pHead p) . vel -< keys
  rec pT <- delay (pTail p) -< pT'
      let p' = playerExtendTail p { pHead = pH, pTail = pT }
          pT' = pTail p'
  returnA -< p'


pos'
    :: HasTime t s
    => V2 Double -> Wire s () IO (Set SDL.Keysym) (V2 Double)
pos' p = integral p . vel


pos
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) (V2 Double)
pos = integral (V2 200 100) . vel


vel
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) (V2 Double)
vel = liftA (uncurry V2) $ velX &&& velY


vel'
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) (V2 Double)
vel' = proc keys -> do
  vx <- velX  -< keys
  vy <- velY' -< keys
  returnA -< V2 vx vy


vel''
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) (V2 Double)
vel'' = proc keys -> do
  vx <-  (-200) . when (isKeyDown Key.ScancodeA)
     <|>   200  . when (isKeyDown Key.ScancodeD)
     <|> 0
          -< keys
  vy <-  (-200) . when (isKeyDown Key.ScancodeW)
     <|>   200  . when (isKeyDown Key.ScancodeS)
     <|> 0
          -< keys
  returnA -< V2 vx vy


velX
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) Double
velX =
        (-200) . when (isKeyDown Key.ScancodeA)
    <|> 200 . when (isKeyDown Key.ScancodeD)
    <|> 0

velX'
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) Double
velX' = proc keys -> do
  let l = isKeyDown Key.ScancodeA keys
      r = isKeyDown Key.ScancodeD keys
  returnA -< if l then (-200) else if r then 200 else 0


velY
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) Double
velY =  pure (-200) . when (isKeyDown Key.ScancodeW)
    <|> pure   200  . when (isKeyDown Key.ScancodeS)
    <|> pure 0


velY'
    :: HasTime t s
    => Wire s () IO (Set SDL.Keysym) Double
velY' =  for 1 . pure (-200) --> for 1 . pure 0   --> velY' . when (isKeyDown Key.ScancodeW)
     <|> for 1 . pure     0  --> for 1 . pure 200 --> velY' . when (isKeyDown Key.ScancodeS)
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


drawPlayer :: SDL.Renderer -> Player -> IO ()
drawPlayer r Player{..} = do
  let hd = Just $ SDL.Rectangle (P $ fmap round pHead) (V2 30 30)
      tl = fmap (\p -> Just $ SDL.Rectangle (P $ fmap round p) (V2 10 10)) pTail
  sequence_ (fmap (SDL.fillRect r) tl)
  SDL.fillRect r hd
  return ()


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
    SDL.rendererDrawColor r $= pure maxBound
    SDL.clear r
    SDL.rendererDrawColor r $= V4 0 0 maxBound maxBound
    drawPlayer r p'
    SDL.present r
    SDL.delay (1000 `div` 60)
    runGameWire ev r s' w'


player :: Player
player = Player { pHead = (V2 300 300)
                , pTail = [(V2 100 100)]
                , pDist = 1
                }


main :: IO ()
main = do
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
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            }
    -- _ <- runGameWire Set.empty renderer clockSession_ $ pos' (V2 300 300)
    -- _ <- runGameWire Set.empty renderer clockSession_ $ plpos' player
    _ <- runGameWire Set.empty renderer clockSession_ $ plWire player
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
