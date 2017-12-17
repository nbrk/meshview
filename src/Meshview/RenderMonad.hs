module Meshview.RenderMonad where

import           Control.Monad.State
import           Data.Map
import           Foreign.Storable
import           Graphics.Rendering.OpenGL hiding (Color, get)
import           Linear
import           Prelude                   hiding (lookup)

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

  unless (member n (rsObjectDict rs)) $ do
    -- upload the object's vertices' data used as "Position" in shaders
    vao <- lift $ setupArrayObject (rsProgram rs) dat dim "Position"

    -- create the param
    let param = ObjectParam
          { opColor = fromColor col
          , opPrimitiveMode = prim
          , opPolygonMode = poli
          , opSize = fromIntegral $ length dat
          , opModelMatrix = transpose identity
          , opVisible = True
          }

    -- insert into the dict
    let dict' = insert n (vao, param) (rsObjectDict rs)

    lift $ putStrLn $ "renderer: addCustomObject " ++ n
    put rs { rsObjectDict = dict'}

rotateXYZ :: String -> (Float, Float, Float) -> Render
rotateXYZ n (rx,ry,rz) = do
  rs <- get

  let q = axisAngle (V3 rx ry rz) 1
  let m = mkTransformation q (V3 1 1 1)
  let dict = rsObjectDict rs
  let dict' = adjust (\(v,p) -> (v, p {opModelMatrix = m})) n dict

  put rs { rsObjectDict = dict'}


rotateX :: String -> Float -> Render
rotateX n rx = do
  rs <- get

  let q = axisAngle (V3 1 0 0) (rx * (180 / pi))
  let m = mkTransformation q (V3 0 0 0)
  let dict = rsObjectDict rs
  let dict' = adjust (\(v,p) -> (v, p {opModelMatrix = m})) n dict

  put rs { rsObjectDict = dict'}

-- loadTriangles :: String -> [(Float, Float, Float)] -> Render
-- loadTriangles n vs = return ()
