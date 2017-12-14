module Main where

import           Meshview.Display
import           Meshview.Simulate
import           Meshview.Types

--import           System.Random


data World = World Float


worldToModel :: World -> Model
worldToModel (World _) = Models [] -- XXX


stepWorld :: Float -> World -> World
stepWorld nsec (World _) = World nsec


main :: IO ()
main = do
  let disp = InWindow (640, 480) (0, 0)
  let ctrl = WithoutMouse
  let bgcol = RGBA 0.2 0.2 0.3 1
  let model = Model "la" (Points [])

--  display disp ctrl bgcol model
  simulate disp ctrl bgcol 1 (World 100) worldToModel stepWorld
