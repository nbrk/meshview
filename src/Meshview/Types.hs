module Meshview.Types where

import           Control.Monad.State
import           Data.Map
import           Graphics.Rendering.OpenGL hiding (Color)
import           Linear


-- | Display configuration
data Display =
  InWindow (Int, Int) (Int, Int)
  | FullScreen (Int, Int)


-- | Controls configuration
data Controls =
  WithMouse
  | WithoutMouse
  deriving Eq


-- | Internal colors
data Color =
  RGBA Float Float Float Float
  -- | White
  -- | Black
  -- | Grey


type Render = StateT RenderState IO ()

data RenderState = RenderState
  { rsProgram          :: Program
  , rsColor            :: Color4 GLfloat
  , rsObjectDict       :: Map String (VertexArrayObject, ObjectParam)
  , rsViewMatrix       :: M44 Float
  , rsProjectionMatrix :: M44 Float
  }

data ObjectParam = ObjectParam
  { opColor         :: Color4 GLfloat
  , opPrimitiveMode :: PrimitiveMode
  , opPolygonMode   :: PolygonMode
  , opSize          :: GLsizei
  , opModelMatrix   :: M44 Float
  , opVisible       :: Bool
  }


data CameraState = CameraState
  { csPos :: V3 Float
  , csDir :: V3 Float
  , csUp  :: V3 Float
  } deriving Show


--- XXX
debug = False
putStrLn' s = when debug (putStrLn s)
