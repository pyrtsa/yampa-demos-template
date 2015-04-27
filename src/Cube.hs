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

tap :: SF AppInput (Event ())
tap = lbp

-- << State datatypes >> -------------------------------------------------------
data Game = Game { gameCube       :: Cube
                 , gamePipe       :: Pipe
                 }
initGame :: Game
initGame = Game initCube initPipe

data Cube = Cube { cubePosY :: Double -- ^ Cube's vertical position
                 , cubeVelY :: Double -- ^ Cube's vertical velocity
                 }
initCube :: Cube
initCube = Cube (winHeight / 2) 0

data Pipe = Pipe { positionX :: Double
                 , height    :: Double
                 }

initPipe :: Pipe
initPipe = Pipe winWidth 300

-- < Cube signal functions > ---------------------------------------------------
-- | Falling Cube just falls down. Doesn't react to anything as can be seen from type.
fallingCube :: Cube -> SF a Cube
fallingCube (Cube p0 v0) = lift2 Cube pos vel
    where vel = constant (-200)  >>> imIntegral v0
          pos = vel              >>> imIntegral p0

-- | Yampy Cube flaps on a command received from AppInput
yampyCube :: Cube -> SF AppInput Cube
yampyCube b0 = kSwitch (fallingCube b0)
                       (first tap >>^ uncurry tag)
                       (\_old (Cube p v) -> yampyCube (Cube p (v + 400)))
              
-- < Pipe signal functions > ---------------------------------------------------

-- | Just a pipe. Moves from right to left.
pipe :: Pipe -> SF a Pipe
pipe (Pipe p0 h0) = pipeHeightGen >>> switch initial (\h -> pipe $ Pipe p0 h)
    where initial = proc h -> do
              p <- imIntegral p0 -< -100
              ev <- edge -< (p < -pipeWidth)
              returnA -< (Pipe p h0, ev `tag` h)

pipeHeightGen :: SF a Double
pipeHeightGen = noiseR (0 + groundHeight + 20, winHeight - pipeGap - 20)
                       (mkStdGen 3) -- FIXME: Make truly random 

-- < The whole game > ----------------------------------------------------------
-- Demo consists of three stages:
-- 1. Intro -- Cube is freezed at initial position
intro :: SF a Game
intro = constant initGame

-- 2. Game  -- Pipes appear, cube falls down flapping when user taps
game :: SF AppInput Game
game = lift2 Game (yampyCube initCube) (pipe initPipe)

-- 3. GameOver -- The screen is freezed displaying the state at the moment of 
--    collision
gameOver :: Game -> SF AppInput Game
gameOver collisionState = constant collisionState

-- | We then switch between these stages
demo :: SF AppInput Game
demo = switch (intro &&& tap) $ \_ ->
       switch (game >>> isGameOver) $ \g ->
       switch (gameOver g &&& tap) $ \_ ->
       demo
    where isGameOver = proc g -> do
              collisionEvent <- edge <<< arr checkCollision -< g
              returnA -< (g, collisionEvent `tag` g)

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
                            (parseWinInput >>> ((demo >>^ render) &&& handleExit))

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
