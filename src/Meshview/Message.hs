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
  | MsgCameraData CameraData
  | MsgGUIDamaged
  | MsgUserData Render
  | MsgGUIForward -- ...
  | MsgGUIBackwards
  | MsgGUIVertAngle



-- type SceneData = [(String, InternalModel)]



-- data PrimitiveType = Points | Triangles | Lines

-- data InternalModel = InternalModel
--    { imodelVertices    :: [V3 Float]
--    , imodelPrimitive   :: PrimitiveType
--    , imodelModelMatrix :: M44 Float
--    , imodelViewMatrix  :: M44 Float
-- --   , imodel TODO
--    }
