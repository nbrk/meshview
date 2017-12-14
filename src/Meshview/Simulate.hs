module Meshview.Simulate where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast

import           Meshview.Camera
import           Meshview.GUI
import           Meshview.Renderer
import           Meshview.Scene
import           Meshview.Timer
import           Meshview.Types


simulate :: Display -> Controls -> Color -> Int -> w -> (w -> Model) -> (Float -> w -> w) -> IO ()
simulate disp ctrl bgcol hz startw wtom stepw = do
  putStrLn "simulate: start"

  runNanoErl $
    spawnGroup [ actorCamera
               , actorScene
               , actorRenderer bgcol -- (RGBA 0.2 0.2 0.3 1)
               , actorGUI disp ctrl
               , actorTimer hz startw wtom stepw
               ]

  putStrLn "simulate: end"
  return ()
