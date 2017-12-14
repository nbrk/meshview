module Meshview.Message where

import qualified Graphics.UI.GLFW as GLFW
import           Linear

import           Meshview.Types


-- | Abstract messages passed between actors (subsystems)
data Message =
  MsgQuit
  | MsgGUIActive
  | MsgRendererActive
  | MsgSceneData SceneData
  | MsgRenderingDone
  | MsgCameraData CameraData
  | MsgGUIDamaged
  | MsgUserData Model
  | MsgGUIForward -- ...
  | MsgGUIBackwards
  | MsgGUIVertAngle



type SceneData = [(String, InternalModel)]


data CameraData = CameraData
  { cameraPos :: V3 Float
  , cameraDir :: V3 Float
  , cameraUp  :: V3 Float
  }


data PrimitiveType = Points | Triangles | Lines

data InternalModel = InternalModel
   { imodelVertices    :: [V3 Float]
   , imodelPrimitive   :: PrimitiveType
   , imodelModelMatrix :: M44 Float
   , imodelViewMatrix  :: M44 Float
--   , imodel TODO
   }
