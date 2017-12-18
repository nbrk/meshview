module Meshview.MeshSTL where

import           Control.Monad.State
import qualified Data.ByteString           as B
import           Data.Map
import           Data.Maybe
import qualified Data.Serialize            as S
import qualified Graphics.Formats.STL      as STL
import           Graphics.Rendering.OpenGL (PolygonMode (..),
                                            PrimitiveMode (..))
import           Linear

import           Meshview.RenderMonad
import           Meshview.Types



meshFromSTL :: FilePath -> String -> Color -> PolygonMode -> Render
meshFromSTL fp n col poly = do
  rs <- get

  unless (member n (rsObjectDict rs)) $ do
    bs <- lift $ B.readFile fp
    case (S.decode bs :: Either String STL.STL) of
      Left err -> error $ "Can't decode STL file: " ++ err
      Right stl -> do
        let dat = concatMap verticesOfTriangle (STL.triangles stl)
        mesh n 3 dat col Triangles poly


verticesOfTriangle :: STL.Triangle -> [V3 Float]
verticesOfTriangle (STL.Triangle _normal vs) =
  let (  (v1x, v1y, v1z)
        ,(v2x, v2y, v2z)
        ,(v3x, v3y, v3z)) = vs
  in
    [ V3 v1x v1y v1z
    , V3 v2x v2y v2z
    , V3 v3x v3y v3z
    ]


