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
  | MsgRenderingRequest
  | MsgCameraData CameraState
  | MsgGUIDamaged
  | MsgUserData Render
  -- GUI input
  | MsgGUIForward -- ...
  | MsgGUIBackwards
  | MsgGUILeft
  | MsgGUIRight
  | MsgGUIUp
  | MsgGUIDown
  | MsgGUITurnLeft
  | MsgGUITurnRight
  | MsgGUIVertHorizAngles Double Double

