module Input
    ( AppInput
    , parseWinInput
    , mousePos
    , lbp
    , lbpPos
    , lbDown
    , rbp
    , rbpPos
    , rbDown
    , quitEvent
    ) where

import           Data.Maybe

import           FRP.Yampa

import           Linear (V2(..))
import           Linear.Affine (Point(..))

import qualified SDL

import Types

-- <| Signal Functions |> --

-- | Current mouse position
mousePos :: SF AppInput (Double,Double)
mousePos = arr inpMousePos

-- | Events that indicate left button click
lbp :: SF AppInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF AppInput (Event (Double,Double))
lbpPos = inpMouseLeft ^>> edgeJust

-- | Is left button down
lbDown :: SF AppInput Bool
lbDown = arr (isJust . inpMouseLeft)

-- | Events that indicate right button click
rbp :: SF AppInput (Event ())
rbp = rbpPos >>^ tagWith ()

-- | Events that indicate right button click and are tagged with mouse position
rbpPos :: SF AppInput (Event (Double,Double))
rbpPos = inpMouseRight ^>> edgeJust

-- | Is right button down
rbDown :: SF AppInput Bool
rbDown = arr (isJust . inpMouseRight)

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge

-- | Exported as abstract type. Fields are accessed with signal functions.
data AppInput = AppInput
    { inpMousePos   :: (Double, Double)        -- ^ Current mouse position
    , inpMouseLeft  :: Maybe (Double, Double)  -- ^ Left button currently down
    , inpMouseRight :: Maybe (Double, Double)  -- ^ Right button currently down
    , inpQuit       :: Bool                    -- ^ SDL's QuitEvent
    }

initAppInput :: AppInput
initAppInput = AppInput { inpMousePos   = (0, 0)
                        , inpMouseLeft  = Nothing
                        , inpMouseRight = Nothing
                        , inpQuit       = False
                        }


-- | Filter and transform SDL events into events which are relevant to our
--   application
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

-- | Compute next input
--   FIXME: I am reinventing lenses once again
nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp SDL.QuitEvent = inp { inpQuit = True }
nextAppInput inp (SDL.MouseMotionEvent { SDL.mouseMotionEventPos = P (V2 x y) }) =
    inp { inpMousePos = (fromIntegral x, fromIntegral y) }
nextAppInput inp ev@(SDL.MouseButtonEvent{}) = inp { inpMouseLeft  = lmb
                                                   , inpMouseRight = rmb }
    where motion = SDL.mouseButtonEventMotion ev
          button = SDL.mouseButtonEventButton ev
          pos    = inpMousePos inp
          inpMod = case (motion,button) of
              (SDL.MouseButtonUp, SDL.ButtonLeft)    -> first (const Nothing)
              (SDL.MouseButtonDown, SDL.ButtonLeft)  -> first (const (Just pos))
              (SDL.MouseButtonUp, SDL.ButtonRight)   -> second (const Nothing)
              (SDL.MouseButtonDown, SDL.ButtonRight) -> second (const (Just pos))
              _                                      -> id
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp

nextAppInput inp _ = inp
