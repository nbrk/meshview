module Meshview.CameraActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Linear                               hiding ((!*))

import           Meshview.Message
import           Meshview.Types


initialCameraData =
  CameraData
  { cdPos = V3 0 0 1
  , cdDir = V3 0 0 0
  , cdUp = V3 0 1 0
  }


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
          MsgCameraData initialCameraData

      MsgGUIForward -> do
        putStrLn "actorCamera: got MsgGUIForward"
        -- XXX
        gref !*
          MsgCameraData initialCameraData
      MsgGUIVertAngle -> do
        putStrLn "actorCamera: got MsgGUIVertAngle"
        -- XXX
        gref !*
          MsgCameraData initialCameraData

      _ -> return ()
