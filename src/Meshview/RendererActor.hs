module Meshview.RendererActor where

import           Control.Concurrent
import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Data.Map
import           Graphics.Rendering.OpenGL            hiding (lookAt,
                                                       perspective)
import           Linear                               hiding ((!*))

import           Meshview.CameraActor                 (initialCameraState)
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
  putStrLn' "actorRenderer: enter"
--  threadDelay 100000
  rs <- initialRenderState
  let rs' = setMatricesFromCamera initialCameraState rs
--  putStrLn' "actorRenderer: initial rs created"

  loop rs' gref mypid
  putStrLn' "actorRenderer: loop fallthrough"


loop :: RenderState -> GroupProcess Message
loop rs gref mypid =
  mypid `receive`
    \msg -> case msg of
      MsgQuit -> do
        putStrLn' "actorRenderer: got MsgQuit, suicide"
        kill mypid
      MsgGUIActive -> do
        putStrLn' "actorRenderer: got MsgGUIActive"
        render rs -- XXX
        gref !* MsgRendererActive
        loop rs gref mypid
      MsgSceneData r -> do
        putStrLn' "actorRenderer: got MsgSceneData with Render"
        rs' <- execRender r rs
        render rs'
        gref !* MsgRenderingDone
        loop rs' gref mypid
      MsgCameraData cs -> do
        putStrLn' "actorRenderer: got MsgCameraData"
        let rs' = setMatricesFromCamera cs rs
        render rs' -- XXX
        gref !* MsgRenderingDone
        loop rs' gref mypid
      MsgGUIDamaged -> do
        putStrLn' "actorRenderer: got MsgGUIDamaged"
        render rs
        gref !* MsgRenderingDone
        loop rs gref mypid
      _ -> loop rs gref mypid


setMatricesFromCamera :: CameraState -> RenderState -> RenderState
setMatricesFromCamera cs rs =
  let pm = perspective
           (45 * (pi / 180))
           (800/600) --(4 / 3)
           0.1
           100000
      vm = lookAt
           (csPos cs)
           (csPos cs ^+^ csDir cs)
           (csUp cs)
  in
    rs { rsProjectionMatrix = transpose pm
       , rsViewMatrix = transpose vm
       }
