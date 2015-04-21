{-# LANGUAGE Arrows, OverloadedStrings #-}
import FRP.Yampa
import FRP.Yampa.Point2

import Graphics
import Shapes
import Input
import Data.Colour.Names

main :: IO ()
main = animate "Demo" 640 480 (parseWinInput >>> demo)

demo :: SF AppInput Object
demo = proc inp -> do
    pos <- mousePos -< inp
    lb <- lbDown -< inp
    returnA -< scene_ [rectangle_ 100 100 ! pos_ pos
                                          ! colour_ (if lb then green else red)]
