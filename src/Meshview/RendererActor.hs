module Meshview.RendererActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import qualified Graphics.Rendering.OpenGL            as GL

import           Meshview.Message
import           Meshview.Types


actorRenderer :: Color -> GroupProcess Message
actorRenderer bgcol gref mypid = forever $
  mypid `receive`
    \msg -> case msg of
      MsgQuit -> do
        putStrLn "actorRenderer: got MsgQuit, suicide"
        kill mypid
      MsgGUIActive -> do
        putStrLn "actorRenderer: got MsgGUIActive"
        gref !* MsgRendererActive
      MsgSceneData r -> do
        putStrLn "actorRenderer: got MsgSceneData with Render"
        let (RGBA r g b a) = bgcol
        GL.clearColor GL.$= GL.Color4 r g b a
        GL.clear [GL.ColorBuffer]
        -- XXX render
        GL.flush
        gref !* MsgRenderingDone
      MsgCameraData cd -> do
        putStrLn "actorRenderer: got MsgCameraData"
        let (RGBA r g b a) = bgcol
        GL.clearColor GL.$= GL.Color4 r g b a
        GL.clear [GL.ColorBuffer]
        -- XXX render
        GL.flush
        gref !* MsgRenderingDone
      MsgGUIDamaged -> do
        putStrLn "actorRenderer: got MsgGUIDamaged"
        let (RGBA r g b a) = bgcol
        GL.clearColor GL.$= GL.Color4 r g b a
        GL.clear [GL.ColorBuffer]
        -- XXX render
        GL.flush
        gref !* MsgRenderingDone
      _ -> return ()
