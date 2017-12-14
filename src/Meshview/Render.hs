module Meshview.Render where

import           Control.Monad
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import           Graphics.Rendering.OpenGL

import           Meshview.Types



render :: RenderState -> IO ()
render rs = do
  clearColor $= rsColor rs
  clear [ ColorBuffer, DepthBuffer ]

  -- uniforms common to the whole scene
  setCommonUniforms rs

  mapM_ (renderObject rs) (rsObjectDict rs)
  flush


renderObject :: RenderState -> (VertexArrayObject, ObjectParam) -> IO ()
renderObject rs (vao, param) =
  unless (opVisible param) $
  do
    bindVertexArrayObject $= Just vao

    setObjectUniforms rs param

    polygonMode $= (opPolygonMode param, opPolygonMode param)
    drawArrays (opPrimitiveMode param) 0 (opSize param)

    bindVertexArrayObject $= Nothing


setCommonUniforms :: RenderState -> IO ()
setCommonUniforms rs = do
  let p = rsProgram rs
  uniformMatrix4fv p "ViewMatrix" (rsViewMatrix rs)
  uniformMatrix4fv p "ProjectionMatrix" (rsProjectionMatrix rs)


setObjectUniforms :: RenderState -> ObjectParam -> IO ()
setObjectUniforms rs param = do
  let p = rsProgram rs
  uniformMatrix4fv p "ModelMatrix" (opModelMatrix param)

  get (uniformLocation p "ObjectColor") >>=
    \l -> uniform l $= opColor param


uniformMatrix4fv :: Storable a => Program -> String -> a -> IO ()
uniformMatrix4fv p n m = do
  loc <- get $ uniformLocation p n
  let loc' = (read . head . tail . words . show) loc
  with m $ \ptr ->
    glUniformMatrix4fv loc' 1 0 (castPtr ptr)


setupArrayObject :: Storable a => Program -> [a] -> Int -> String -> IO VertexArrayObject
setupArrayObject program dat dim n = do
  -- new VAO
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  -- VBO, copy the position data
  withArray dat $ \ptr -> do
    -- bind new buffer
    buf <- genObjectName
    bindBuffer ArrayBuffer $= Just buf

    -- transfer the data
    let size = fromIntegral (length dat * sizeOf (head dat))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw) -- XXX StaticDraw??

    -- point shader atname
    atloc <- get $ attribLocation program n
    vertexAttribPointer atloc  $=
      (ToFloat,
       VertexArrayDescriptor (fromIntegral dim) Float 0 (bufferOffset 0))

    -- enable the location (a block of bytes in the VBO via the above pointer)
    vertexAttribArray atloc $= Enabled

  bindVertexArrayObject $= Nothing
  return vao
  where
    bufferOffset = plusPtr nullPtr . fromIntegral
