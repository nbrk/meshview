module Meshview.Message where

import qualified Graphics.UI.GLFW as GLFW
import           Linear

import           Meshview.Types


-- | Abstract messages passed between actors (subsystems)
data Message =
  MsgQuit
  | MsgGUIActive
  | MsgRendererActive
  | MsgSceneData Render
  | MsgRenderingDone
  | MsgCameraData CameraState
  | MsgGUIDamaged
  | MsgUserData Render
  -- GUI input
  | MsgGUIForward -- ...
  | MsgGUIBackwards
  | MsgGUIVertAngle

