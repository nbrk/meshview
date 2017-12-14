module Meshview.RendererActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Data.Map
import           Graphics.Rendering.OpenGL
import           Linear                               hiding ((!*))

import           Meshview.FragmentShader
import           Meshview.LoadShaders
import           Meshview.Message
import           Meshview.Render
import           Meshview.RenderMonad
import           Meshview.Types
import           Meshview.VertexShader


initialRenderState :: IO RenderState
initialRenderState = do
  p <- loadShaders [
    ShaderInfo VertexShader (StringSource  vertexShaderSourceString),
    ShaderInfo FragmentShader (StringSource fragmentShaderSourceString)]
  currentProgram $= Just p

  return
    RenderState
    { rsProgram = p
    , rsColor = Color4 0.2 0.2 0.3 1
    , rsObjectDict = empty
    , rsViewMatrix = identity :: M44 Float
    , rsProjectionMatrix = identity :: M44 Float
    }




actorRenderer :: GroupProcess Message
actorRenderer gref mypid = do
  -- init shaders, matrices, etc.
  rs <- initialRenderState
  loop rs gref mypid


loop :: RenderState -> GroupProcess Message
loop rs gref mypid =
  mypid `receive`
    \msg -> case msg of
      MsgQuit -> do
        putStrLn "actorRenderer: got MsgQuit, suicide"
        kill mypid
      MsgGUIActive -> do
        putStrLn "actorRenderer: got MsgGUIActive"
        gref !* MsgRendererActive
        loop rs gref mypid
      MsgSceneData r -> do
        putStrLn "actorRenderer: got MsgSceneData with Render"
        rs' <- execRender r rs
        render rs'
        gref !* MsgRenderingDone
        loop rs' gref mypid
      -- MsgCameraData cd -> do
      --   putStrLn "actorRenderer: got MsgCameraData"
      --   let (RGBA r g b a) = bgcol
      --   GL.clearColor GL.$= GL.Color4 r g b a
      --   GL.clear [GL.ColorBuffer]
      --   -- XXX render
      --   GL.flush
      --   gref !* MsgRenderingDone
      MsgGUIDamaged -> do
        putStrLn "actorRenderer: got MsgGUIDamaged"
        render rs
        gref !* MsgRenderingDone
        loop rs gref mypid
      _ -> loop rs gref mypid


