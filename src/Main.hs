{-# LANGUAGE Arrows, OverloadedStrings #-}
import FRP.Yampa
import FRP.Yampa.Utilities
import Data.Colour.Names

import Graphics
import Shapes
import Input

type Scalar = Double
type Vector = (Scalar, Scalar)
type Position = (Scalar, Scalar)

main :: IO ()
main = animate "Demo" 640 480 (parseWinInput >>> ((demo >>> render) &&& handleExit))

-- | A ball will rest until the first click.
--   After the click it starts to fall.
--   The ball can be kicked with another click.
--   Ball has no limits, although you can bring it back.
demo :: SF AppInput Ball
demo = switch (constant ball &&& lbp) (const $ kickableBall ball)

gravity :: Vector
gravity = (0, -200)

data Ball = Ball { position :: Position
                 , velocity :: Vector
                 }
ball :: Ball
ball = Ball (320, 240) (0, 0)

-- | Kicks the ball in the direction of the specified point
kick :: Position -> Ball -> Ball
kick (tx, ty) (Ball p v) = Ball p (v ^+^ impulse)
    where impulse = (tx,ty) ^-^ p

fallingBall :: Ball -> SF a Ball
fallingBall (Ball p0 v0) = lift2 Ball pos vel
    where vel = constant gravity >>> imIntegral v0
          pos = vel              >>> imIntegral p0

kickableBall :: Ball -> SF AppInput Ball
kickableBall b0 =
    kSwitch (fallingBall b0)                       -- initial SF
            (first lbpPos >>^ uncurry attach)      -- switch trigger
            (\_old -> kickableBall . uncurry kick) -- create a new SF

render :: SF Ball Object
render = scene_ . (:[]) ^<< arr renderBall
    -- Here is your wooden rectangular ball, son. Go play with your buddies.
    where renderBall (Ball pos _) =
                circle_ 100 ! pos_ pos
                            ! colour_ red

-- | Returns False when there is a signal to exit
--   You might also want to handle other signals here (e.g. Esc button press)
handleExit :: SF AppInput Bool
handleExit = quitEvent >>> arr isEvent
