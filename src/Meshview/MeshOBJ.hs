module Meshview.MeshOBJ where

import qualified Codec.Wavefront           as OBJ
import           Control.Monad.State
import qualified Data.ByteString           as B
import           Data.Maybe
import qualified Data.Serialize            as S
import qualified Data.Vector               as V
import           Graphics.Rendering.OpenGL (PolygonMode (..),
                                            PrimitiveMode (..))
import           Linear

import           Meshview.RenderMonad
import           Meshview.Types



meshFromOBJ :: FilePath -> String -> Color -> PolygonMode -> Render
meshFromOBJ fp n col poly = do
  e <- lift $ OBJ.fromFile fp
  case e of
    Left err -> error $ "Can't load OBJ file: " ++ err
    Right obj -> let
      dat = V.toList $ fmap verticeFromLocation (OBJ.objLocations obj)
      in
        mesh n 3 dat col Triangles poly


verticeFromLocation :: OBJ.Location -> V3 Float
verticeFromLocation (OBJ.Location x y z _w) = V3 x y z
