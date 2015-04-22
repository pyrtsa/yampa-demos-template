{-# LANGUAGE Arrows, OverloadedStrings #-}
import FRP.Yampa
import FRP.Yampa.Point2
import FRP.Yampa.Utilities
import Data.Colour.Names

import Graphics
import Shapes
import Input

type Scalar = Double
type Vector = (Scalar, Scalar)
type Position = (Scalar, Scalar)

main :: IO ()
main = animate "Demo" 640 480 (parseWinInput >>> demo >>> render)

-- | A ball will rest until the first click.
--   After the click it starts to fall.
--   The ball can be kicked with another click.
--   Ball has no limits.
demo :: SF AppInput Ball
demo = switch (constant ball &&& lbp) (const $ kickableBall ball)

gravity :: Vector
gravity = (0, -200)

data Ball = Ball { position :: Position
                 , velocity :: Vector
                 }
ball :: Ball
ball = Ball (320, 240) (0, 0)

kick :: Ball -> Ball
kick (Ball p v) = Ball p (v ^+^ (0, 400))

fallingBall :: Ball -> SF a Ball
fallingBall (Ball p0 v0) = lift2 Ball pos vel
    where vel = constant gravity >>> integral >>^ (^+^ v0)
          pos = vel              >>> integral >>^ (^+^ p0)

kickableBall :: Ball -> SF AppInput Ball
kickableBall b0 = kSwitch (fallingBall b0)               -- initial SF
                          (first lbp >>^ uncurry tag)    -- switch trigger
                          (\_old -> kickableBall . kick) -- create a new SF

render :: SF Ball Object
render = scene_ . (:[]) ^<< arr renderBall
    -- Here is your wooden rectangular ball, son. Go play with your buddies.
    where renderBall (Ball (x,y) _) =
                rectangle_ 100 100 ! pos_ (Point2 (640 - x) (480 - y))
                                   ! colour_ red
