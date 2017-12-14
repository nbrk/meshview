module Meshview.CameraActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Linear                               hiding ((!*))

import           Meshview.Message
import           Meshview.Types



actorCamera :: GroupProcess Message
actorCamera gref mypid = forever $
  mypid `receive`
    \msg -> case msg of
      MsgQuit -> do
        putStrLn "actorCamera: got MsgQuit, suicide"
        kill mypid
      MsgRendererActive -> do
        putStrLn "actorCamera: got MsgRendererActive"
        gref !*
          MsgCameraData (CameraData (V3 0 0 1) (V3 0 0 0) (V3 0 1 0))
      MsgGUIForward -> do
        putStrLn "actorCamera: got MsgGUIForward"
        -- XXX
        gref !*
          MsgCameraData (CameraData (V3 0 0 1) (V3 0 0 0) (V3 0 1 0))
      MsgGUIVertAngle -> do
        putStrLn "actorCamera: got MsgGUIVertAngle"
        -- XXX
        gref !*
          MsgCameraData (CameraData (V3 0 0 1) (V3 0 0 0) (V3 0 1 0))
      _ -> return ()
