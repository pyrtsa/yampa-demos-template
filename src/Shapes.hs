module Shapes where

import FRP.Yampa.AffineSpace

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
                     , objColor :: Colour Double
                     } deriving (Show, Eq)

instance Default Object where
    def = Object { objShape = error "Object shape wasn't define"
                 , objPos   = origin
                 , objColor = white
                 }

-- It might worth to use lenses here in order to avoid building a
-- poor version of them

scene_ :: [Object] -> Object
scene_ objs = def { objShape = Scene objs, objColor = black}

circle_ :: Int -> Object
circle_ n = def { objShape = Circle n }

rectangle_ :: Int -> Int -> Object
rectangle_ x y = def { objShape = Rectangle x y }

type AttributeSetter = Object -> Object

pos_ :: Position2 -> AttributeSetter
pos_ pos obj = obj { objPos = pos }

color_ :: Colour Double -> AttributeSetter
color_ color obj = obj { objColor = color }

(!) :: Object -> AttributeSetter -> Object
(!) = flip ($)