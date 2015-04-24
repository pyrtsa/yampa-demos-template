module Graphics
    ( animate
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Colour.SRGB (toSRGB24, RGB(..))
import           Data.Text (Text)
import qualified Data.Vector.Storable as Vector

import           FRP.Yampa
import           FRP.Yampa.Point2

import           Linear (V2(..), V4(..))
import           Linear.Affine (Point(..))

import qualified SDL

import Shapes
import Types

-- | (Object to render, should the app exit)
--   TODO: Datatype
type WinOutput = (Object, Bool)

animate :: Text                -- ^ window title
        -> Int                 -- ^ window width in pixels
        -> Int                 -- ^ window height in pixels
        -> (SF WinInput WinOutput) -- ^ signal function to animate
        -> IO ()
animate title width height sf = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) rendererConf
    SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (obj, shouldExit) = do
            when changed $ do
                renderObject renderer obj
                SDL.renderPresent renderer
            return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where windowConf =
              SDL.defaultWindow { SDL.windowSize = V2 (fromIntegral width)
                                                      (fromIntegral height) }
          rendererConf = SDL.RendererConfig
              { SDL.rendererAccelerated   = True
              , SDL.rendererSoftware      = False
              , SDL.rendererTargetTexture = False
              , SDL.rendererPresentVSync  = False
              }

renderObject :: SDL.Renderer -> Object -> IO ()
renderObject renderer obj = setRenderAttrs >> renderShape
    where setRenderAttrs = do
              let (RGB r g b) = toSRGB24 $ objColour obj
              SDL.setRenderDrawColor renderer (V4 r g b maxBound)
          renderShape = case objShape obj of
              Rectangle x y -> SDL.renderDrawRect renderer $
                                     SDL.Rectangle (P (V2 (toEnum $ floor px)
                                                          (toEnum $ floor py)))
                                                   (V2 (toEnum x) (toEnum y))
              Scene objs -> do
                  SDL.renderClear renderer
                  mapM_ (renderObject renderer) objs
              Circle r -> SDL.renderDrawPoints renderer $ Vector.fromList $
                                map (\(x,y) -> P (V2 (toEnum x) (toEnum y))) $
                                rasterCircle (floor px, floor py) r
          (Point2 px py) = objPos obj

rasterCircle :: (Num a, Ord a) => (a,a) -> a -> [(a,a)]
rasterCircle (x0,y0) radius = step 0 radius (5 - 4 * radius)
    where step x y m = (x0 + x, y0 + y)
                     : (x0 + y, y0 + x)
                     : (x0 - x, y0 + y)
                     : (x0 - y, y0 + x)
                     : (x0 + x, y0 - y)
                     : (x0 + y, y0 - x)
                     : (x0 - x, y0 - y)
                     : (x0 - y, y0 - x)
                     : cont
            where cont | x <= y' = step (x+1) y' (m' + 8 * x + 4)
                       | otherwise = []
                  (y',m') | m > 0 = (y-1, m - 8 * y)
                          | otherwise = (y,m)
