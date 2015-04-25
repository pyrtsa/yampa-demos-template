module Shapes where

import FRP.Yampa.AffineSpace
import FRP.Yampa.Point2 (Point2(..))

import Data.Default
import Data.Colour
import Data.Colour.Names

import Types

type Frame = Object

data Shape = Circle Int
           | Rectangle Int Int
           | Scene [Object] -- ^ Not really a shape
           deriving (Show, Eq)

data Object = Object { objShape :: Shape
                     , objPos   :: Position2
                     , objColour :: Colour Double
                     } deriving (Show, Eq)

instance Default Object where
    def = Object { objShape = error "Object shape wasn't define"
                 , objPos   = origin
                 , objColour = white
                 }

-- Terms are written with postfix '_' indicating data rather than code.
-- (stolen from lucid)

-- It might worth to use lenses here in order to avoid building a
-- poor version of them

scene_ :: [Object] -> Object
scene_ objs = def { objShape = Scene objs, objColour = black}

circle_ :: Double -> Object
circle_ n = def { objShape = Circle (round n) }

rectangle_ :: Double -> Double -> Object
rectangle_ x y = def { objShape = Rectangle (round x) (round y) }

type AttributeSetter = Object -> Object

pos_ :: (Double, Double) -> AttributeSetter
pos_ (x, y) obj = obj { objPos = Point2 x y}

colour_ :: Colour Double -> AttributeSetter
colour_ colour obj = obj { objColour = colour }

(!) :: Object -> AttributeSetter -> Object
(!) = flip ($)
