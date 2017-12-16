module Meshview.CameraActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Linear                               hiding ((!*))

import           Meshview.Message
import           Meshview.Types


initialCameraState =
  CameraState
  { csPos = V3 0 0 7
  , csDir = V3 0 0 (-0.1)
  , csUp = V3 0 1 0
  }


actorCamera :: GroupProcess Message
actorCamera = loop initialCameraState

loop :: CameraState -> GroupProcess Message
loop cs gref mypid =
  mypid `receive`
    \msg -> case msg of
      MsgQuit -> do
        putStrLn' "actorCamera: got MsgQuit, suicide"
        kill mypid
      -- MsgRendererActive -> do
      --   putStrLn' "actorCamera: got MsgRendererActive (bcast initial cs)"
      --   gref !* MsgCameraData cs
      --   loop cs gref mypid

      MsgGUIForward -> do
        putStrLn' "actorCamera: got MsgGUIForward"
        -- XXX change cs
        let cs' = forward cs
        putStrLn' $ "actorCamera: MsgCameraData: " ++ show cs'
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUIBackwards -> do
        putStrLn' "actorCamera: got MsgGUIBackwards"
        -- XXX change cs
        let cs' = backwards cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      -- MsgGUIVertAngle -> do
      --   putStrLn' "actorCamera: got MsgGUIVertAngle"
      --   -- XXX change cs
      --   gref !* MsgCameraData cs
      --   loop cs gref mypid

      _ -> loop cs gref mypid


forward :: CameraState -> CameraState
forward cs =
  cs { csPos = csPos cs ^+^ csDir cs}


backwards :: CameraState -> CameraState
backwards cs =
  cs { csPos = csPos cs ^-^ csDir cs}
