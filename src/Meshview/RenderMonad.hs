module Meshview.RenderMonad where

import           Control.Monad.State
import           Data.Map
import           Foreign.Storable
import           Graphics.Rendering.OpenGL (PolygonMode (..),
                                            PrimitiveMode (..))
import           Linear                    hiding (rotate)
import           Prelude                   hiding (lookup)

import           Meshview.Color
import           Meshview.Render
import           Meshview.Types


execRender :: Render -> RenderState -> IO RenderState
execRender = execStateT


--
-- User interface
--

-- | Add an object with custom mesh and params (noop if the name already exists)
mesh :: Storable a => String -> Int -> [a] -> Color -> PrimitiveMode -> PolygonMode -> Render
mesh n dim dat col prim poly = do
  rs <- get

  unless (member n (rsObjectDict rs)) $ do
    -- upload the object's vertices' data used as "Position" in shaders
    vao <- lift $ setupArrayObject (rsProgram rs) dat dim "Position"

    -- create the param
    let param = ObjectParam
          { opColor = fromColor col
          , opPrimitiveMode = prim
          , opPolygonMode = poly
          , opSize = fromIntegral $ length dat
          , opTranslationVector = V3 0 0 0
          , opRotationQ = axisAngle (V3 0 0 0) 0
          , opVisible = True
          }

    -- insert into the dict
    let dict' = insert n (vao, param) (rsObjectDict rs)

    lift $ putStrLn $ "renderer: new mesh `" ++ n ++ "`"
    put rs { rsObjectDict = dict'}



-- | Rotate and translate an object (if any). The x y z angles are in degrees
rotateTranslateXYZ :: String -> Maybe (Float, Float, Float) -> Maybe (Float, Float, Float) -> Render
rotateTranslateXYZ n mbr mbt = do
  rs <- get

  case (lookup n (rsObjectDict rs)) of
    Just (vao, param) -> do
      let q =  case mbr of
                 Just (rx,ry,rz) ->
                   -- XXX make rotation quaternions
                   let qx = axisAngle (V3 1 0 0) (rx * (180 / pi))
                       qy = axisAngle (V3 0 1 0) (ry * (180 / pi))
                       qz = axisAngle (V3 0 0 1) (rz * (180 / pi))
                   in
                     qx * qy * qz
                 Nothing ->
                   -- If the rotation is not specified, get the old q
                   opRotationQ param
      let t = case mbt of
                -- Overwrite the translation vector or keep the old one
                Just (tx, ty, tz) -> V3 tx ty tz
                Nothing           -> opTranslationVector param

      -- update the dict
      let dict' = adjust (\(v,p) -> (v, p {opRotationQ = q, opTranslationVector = t})) n (rsObjectDict rs)
      put rs { rsObjectDict = dict'}

    -- noop if there is no such an object
    Nothing -> return ()


-- | Set the object's rotation (x, y, z angles in degrees)
rotate n (rx,ry,rz) = rotateTranslateXYZ n (Just (rx, ry, rz)) Nothing
rotateX n r = rotate n (r, 0, 0)
rotateY n r = rotate n (0, r, 0)
rotateZ n r = rotate n (0, 0, r)


-- | Translate the object wrt its center
translate n (tx, ty, tz) = rotateTranslateXYZ n Nothing (Just (tx, ty, tz))
translateX n t = translate n (t, 0, 0)
translateY n t = translate n (0, t, 0)
translateZ n t = translate n (0, 0, t)


