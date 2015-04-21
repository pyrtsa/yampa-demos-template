{-# LANGUAGE RecordWildCards #-}
module Graphics
    ( animate
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Colour.SRGB (toSRGB24, RGB(..))
import           Data.IORef
import           Data.Text (Text)

import           FRP.Yampa
import           FRP.Yampa.Point2

import           Linear (V2(..), V4(..))
import           Linear.Affine (Point(..))

import qualified SDL

import Shapes
import Input

animate :: Text                -- ^ window title
        -> Int                 -- ^ window width in pixels
        -> Int                 -- ^ window height in pixels
        -> (SF WinInput Object) -- ^ signal function to animate
        -> IO ()
animate title width height sf = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) rendererConf
    SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

    shouldStop      <- newIORef False
    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent
            writeIORef shouldStop (Just SDL.QuitEvent == (SDL.eventPayload <$> mEvent))
            return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed obj = do
            when changed $ do
                renderObject renderer obj
                SDL.renderPresent renderer
            readIORef shouldStop

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
renderObject renderer Object{..} = setRenderAttrs >> renderShape
    where setRenderAttrs = do
              let (RGB r g b) = toSRGB24 objColor
              SDL.setRenderDrawColor renderer (V4 r g b maxBound)
          renderShape = case objShape of
              Rectangle x y -> let (Point2 px py) = objPos in
                  SDL.renderDrawRect renderer $
                                     SDL.Rectangle (P (V2 (toEnum $ floor px)
                                                          (toEnum $ floor py)))
                                                   (V2 (toEnum x) (toEnum y))
              Scene objs -> do
                  SDL.renderClear renderer
                  mapM_ (renderObject renderer) objs
              Circle{..} -> error "Not implemented"
