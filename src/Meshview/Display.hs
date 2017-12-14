module Meshview.Display where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast

import           Meshview.Camera
import           Meshview.GUI
import           Meshview.Renderer
import           Meshview.Scene
import           Meshview.Timer
import           Meshview.Types


display :: Display -> Controls -> Color -> Model -> IO ()
display disp ctrl bgcol m = do
  putStrLn "display: start"

  runNanoErl $
    spawnGroup [ actorCamera
               , actorScene
               , actorRenderer (RGBA 0.2 0.2 0.3 1)
               , actorGUI disp ctrl
               ]

  putStrLn "display: end"
  return ()
