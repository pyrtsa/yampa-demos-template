{-# LANGUAGE Arrows, OverloadedStrings #-}
module Main where
import FRP.Yampa
import FRP.Yampa.Utilities
import Data.Colour.SRGB

import Graphics
import Shapes
import Input
import System.Random (mkStdGen)

cubeX, cubeWidth, cubeHeight :: Double
cubeX      = 100
cubeWidth  = 30
cubeHeight = 30

pipeWidth, pipeGap :: Double
pipeWidth = 60
pipeGap   = 200

cubeColour, pipeColour, skyColour, groundColour :: Colour Double
cubeColour    = sRGB24 0xED 0xBA 0x00
pipeColour    = sRGB24 0x1A 0xAF 0x5D
skyColour     = sRGB24 0xAD 0xD4 0xF4
groundColour  = sRGB24 0xCE 0xB1 0x71

winHeight, winWidth :: Double
winHeight = 600
winWidth  = 300

groundHeight :: Double
groundHeight = winHeight / 8

-- << State datatypes >> -------------------------------------------------------
data Game = Game { gameCube       :: Cube
                 , gamePipe       :: Pipe
                 }

data Cube = Cube { cubePosY :: Double -- ^ Cube's vertical position
                 , cubeVelY :: Double -- ^ Cube's vertical velocity
                 }
initCube :: Cube
initCube = Cube (winHeight / 2) 0

data Pipe = Pipe { positionX :: Double
                 , height    :: Double
                 }

-- < Cube signal functions > ---------------------------------------------------
-- | Dumb Cube just falls down. Doesn't react to anything as can be seen from type.
dumbCube :: Cube -> SF a Cube
dumbCube (Cube p0 v0) = lift2 Cube pos vel
    where vel = constant (-200)  >>> imIntegral v0
          pos = vel              >>> imIntegral p0

-- | Yampy Cube flaps by a command received from AppInput
yampyCube :: Cube -> SF AppInput Cube
yampyCube b0 = kSwitch (dumbCube b0)
                       (first lbpPos >>^ uncurry attach)
                       (\_old (_, Cube p v) -> yampyCube (Cube p (v + 400)))
              
-- < Pipe signal functions > ---------------------------------------------------

-- | Just a pipe. Moves from right to left.
pipe :: Pipe -> SF a Pipe
pipe (Pipe p0 h0) = lift2 Pipe pos (constant h0)
    where pos = constant (-100) >>> imIntegral p0

-- | A pipe moves from left to right and then reappears at right.
-- FIXME: There must be a better way to write this
reappearingPipe :: Pipe -> SF a Pipe
reappearingPipe t0 = switch update change
    where update = proc _ -> do
              t <- pipe t0 -< ()
              ev <- edge -< positionX t < 0 - pipeWidth
              h <- pipeHeightGen -< ()
              returnA -< (t, ev `tag` h)
          change h = reappearingPipe (Pipe winWidth h)

pipeHeightGen :: SF a Double
pipeHeightGen = noiseR (0 + groundHeight + 20, winHeight - pipeGap - 20)
                       (mkStdGen 3) -- FIXME: Make truly random 

-- < The whole game > ----------------------------------------------------------

intro :: SF AppInput Game
intro = switch (constant (Game initCube (Pipe winWidth 0)) &&& lbp)
               (const $ game (Game initCube (Pipe winWidth 300)))

game :: Game -> SF AppInput Game
game (Game cube0 pipe0) = kSwitch initial trigger new
    where initial = lift2 Game (yampyCube cube0) (reappearingPipe pipe0)
          trigger = snd ^>> (collision &&& identity) >>^ uncurry tag
          new _old lastState = switch (constant lastState &&& lbp) (const intro)

collision :: SF Game (Event ())
collision = arr checkCollision >>> edge
  where
    checkCollision :: Game -> Bool
    checkCollision (Game (Cube cubeY _) (Pipe pipeX pipeHeight)) =
      or [ collide (pipeHeight, pipeHeight)
         , collide (winHeight, winHeight - pipeHeight - pipeGap)
         , cubeY <= groundHeight + cubeHeight ]
      where collide (y2, h2) = and [ cubeX + cubeWidth  > pipeX
                                   , cubeX              < pipeX + pipeWidth
                                   , cubeY              > y2 - h2
                                   , cubeY - cubeHeight < y2 ]

main :: IO ()
main = animate "Yampy cube" (round winWidth) (round winHeight)
                            (parseWinInput >>> ((intro >>^ render) &&& handleExit))

render :: Game -> Object
render (Game (Cube y _) (Pipe p h)) = scene ! colour_ skyColour
    where scene = scene_ [ground, cube, bottomPipe, upperPipe]
          cube = rectangle_ cubeWidth cubeHeight ! pos_ (cubeX, y)
                                                 ! colour_ cubeColour
          bottomPipe = rectangle_ pipeWidth h ! pos_ (p, h)
                                              ! colour_ pipeColour
          upperPipe = rectangle_ pipeWidth (winHeight - h - pipeGap)
                                              ! pos_ (p, winHeight)
                                              ! colour_ pipeColour
          ground = rectangle_ winWidth groundHeight ! pos_ (0, groundHeight)
                                                    ! colour_ groundColour

handleExit :: SF AppInput Bool
handleExit = quitEvent >>> arr isEvent
