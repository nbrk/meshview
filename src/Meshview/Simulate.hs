module Meshview.Simulate where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast

import           Meshview.CameraActor
import           Meshview.GUIActor
import           Meshview.RendererActor
import           Meshview.SceneActor
import           Meshview.TimerActor
import           Meshview.Types


simulate :: Display -> Controls -> Int -> w -> (w -> Render) -> (Float -> w -> w) -> IO ()
simulate disp ctrl hz startw wtom stepw = do
  putStrLn "simulate: start"

  -- runNanoErl $
  --   spawnGroup [ actorCamera
  --              , actorScene
  --              , actorRenderer -- (RGBA 0.2 0.2 0.3 1)
  --              , actorGUI disp ctrl
  --              , actorTimer hz startw wtom stepw
  --              ]
  runNanoErl $
    spawnGroup [
               actorCamera
               , actorScene
               , actorRenderer
               , actorGUI disp ctrl
               , actorTimer hz startw wtom stepw
               , actorTimerFPSShooter 48
               ]

  putStrLn "simulate: end"
  return ()
