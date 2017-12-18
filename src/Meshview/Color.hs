module Meshview.Color where

import           Graphics.Rendering.OpenGL (Color4 (..))
import           Meshview.Types


fromColor :: Color -> Color4 Float
fromColor (RGBA r g b a) = Color4 r g b a

-- | Integral colours
black, white, red, lime, blue, yellow, cyan, magenta :: Color
black = RGBA 0 0 0 0
white = RGBA 1 1 1 0
red = RGBA 1 0 0 0
lime = RGBA 0 1 0 0
blue = RGBA 0 0 1 0
yellow = RGBA 1 1 0 0
cyan = RGBA 0 1 1 0
magenta = RGBA 1 0 1 0

-- | Fraction colours
silver, gray, maroon, olive, green, purple, teal, navy :: Color
silver = RGBA (3/4) (3/4) (3/4) 0
gray = RGBA (1/2) (1/2) (1/2) 0
maroon = RGBA (1/2) 0 0 0
olive = RGBA (1/2) (1/2) 0 0
green = RGBA 0 (1/2) 0 0
purple = RGBA (1/2) 0 (1/2) 0
teal = RGBA 0 (1/2) (1/2) 0
navy = RGBA 0 0 (1/2) 0
