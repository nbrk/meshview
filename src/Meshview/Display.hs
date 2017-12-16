module Meshview.Display where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast

import           Meshview.CameraActor
import           Meshview.GUIActor
import           Meshview.RendererActor
import           Meshview.SceneActor
import           Meshview.TimerActor
import           Meshview.Types


display :: Display -> Controls -> Render -> IO ()
display disp ctrl r = do
  runNanoErl $
    spawnGroup [
               actorCamera
               , actorScene
               , actorRenderer
               , actorGUI disp ctrl
               , actorUntimer r -- static Render broadcaster
               ]

  return ()
