module Meshview.RenderMonad where

import           Control.Monad.State
import           Data.Map
import           Foreign.Storable
import           Graphics.Rendering.OpenGL hiding (Color, get)
import           Linear

import           Meshview.Color
import           Meshview.Render
import           Meshview.Types


execRender :: Render -> RenderState -> IO RenderState
execRender = execStateT


--
-- User interface
--

addCustomObject :: Storable a => String -> Int -> [a] -> Color -> PrimitiveMode -> PolygonMode -> Render
addCustomObject n dim dat col prim poli = do
  rs <- get

  -- upload the object's vertices' data used as "Position" in shaders
  vao <- lift $ setupArrayObject (rsProgram rs) dat dim "Position"

  -- create the param
  let param = ObjectParam
        { opColor = fromColor col
        , opPrimitiveMode = prim
        , opPolygonMode = poli
        , opSize = fromIntegral $ length dat
        , opModelMatrix = identity
        , opVisible = True
        }

  -- insert into the dict
  let dict' = insert n (vao, param) (rsObjectDict rs)

  put rs { rsObjectDict = dict'}



-- loadTriangles :: String -> [(Float, Float, Float)] -> Render
-- loadTriangles n vs = return ()
