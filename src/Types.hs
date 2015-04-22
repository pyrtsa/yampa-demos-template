module Types where

import FRP.Yampa
import FRP.Yampa.Point2

import qualified SDL

type Position2 = Point2 Double
type WinInput = Event SDL.EventPayload
