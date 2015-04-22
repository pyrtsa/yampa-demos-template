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
    ) where

import           Data.Maybe

import           FRP.Yampa
import           FRP.Yampa.AffineSpace
import           FRP.Yampa.Point2

import           Linear (V2(..))
import           Linear.Affine (Point(..))

import qualified SDL

import Types

-- <| Signal Functions |> --

-- | Current mouse position
mousePos :: SF AppInput Position2
mousePos = arr inpMousePos

-- | Events that indicate left button click
lbp :: SF AppInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF AppInput (Event Position2)
lbpPos = inpMouseLeft ^>> edgeJust

-- | Is left button down
lbDown :: SF AppInput Bool
lbDown = arr (isJust . inpMouseLeft)

-- | Events that indicate right button click
rbp :: SF AppInput (Event ())
rbp = rbpPos >>^ tagWith ()

-- | Events that indicate right button click and are tagged with mouse position
rbpPos :: SF AppInput (Event Position2)
rbpPos = inpMouseRight ^>> edgeJust

-- | Is right button down
rbDown :: SF AppInput Bool
rbDown = arr (isJust . inpMouseRight)

-- | Exported as abstract type. Fields are accessed with signal functions.
data AppInput = AppInput
    { inpMousePos   :: Position2        -- ^ Current mouse position
    , inpMouseLeft  :: Maybe Position2  -- ^ Left button currently down
    , inpMouseRight :: Maybe Position2  -- ^ Right button currently down
    }

initAppInput :: AppInput
initAppInput = AppInput { inpMousePos   = origin
                        , inpMouseLeft  = Nothing
                        , inpMouseRight = Nothing
                        }


-- | Filter and transform SDL events into events which are relevant to our
--   application
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

-- | Compute next input
--   FIXME: I am reinventing lenses once again
nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp (SDL.MouseMotionEvent { SDL.mouseMotionEventPos = P (V2 x y) }) =
    inp { inpMousePos = Point2 (fromIntegral x) (fromIntegral y) }
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
              _                                      -> id *** id
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp

nextAppInput inp _ = inp
