module Meshview.Color where

import           Graphics.Rendering.OpenGL (Color4 (..))
import           Meshview.Types


fromColor :: Color -> Color4 Float
fromColor (RGBA r g b a) = Color4 r g b a

black :: Color
black = RGBA 0 0 0 1

white :: Color
white = RGBA 1 1 1 1

red :: Color
red = RGBA 1 0 0 1

green :: Color
green = RGBA 0 1 0 1

blue :: Color
blue = RGBA 0 0 1 1

