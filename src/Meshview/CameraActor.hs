module Meshview.CameraActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Linear                               hiding ((!*))

import           Meshview.Message
import           Meshview.Types



initialCameraState =
  CameraState
  { csPos = V3 0 0 3
  , csDir = V3 0 0 (-0.01)
  , csUp = V3 0 1 0
--  , csRight = V3 1 0 0
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
        let cs' = forward cs
        putStrLn' $ "actorCamera: MsgCameraData: " ++ show cs'
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUIBackwards -> do
        putStrLn' "actorCamera: got MsgGUIBackwards"
        let cs' = backwards cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUILeft -> do
        putStrLn' "actorCamera: got MsgGUILeft"
        let cs' = left cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUIRight -> do
        putStrLn' "actorCamera: got MsgGUIRight"
        let cs' = right cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUIUp -> do
        putStrLn' "actorCamera: got MsgGUIUp"
        let cs' = up cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUIDown -> do
        putStrLn' "actorCamera: got MsgGUIDown"
        let cs' = down cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUITurnLeft -> do
        putStrLn' "actorCamera: got MsgGUITurnLeft"
        let cs' = turnLeft cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUITurnRight -> do
        putStrLn' "actorCamera: got MsgGUITurnRight"
        let cs' = turnRight cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid
      MsgGUIVertHorizAngles offx offy -> do
        putStrLn' "actorCamera: got MsgGUIVertHorizAngles"
        let cs' = vertHorizAngles (realToFrac offx) (realToFrac offy) cs
        gref !* MsgCameraData cs'
        loop cs' gref mypid

      _ -> loop cs gref mypid


forward :: CameraState -> CameraState
forward cs =
  cs { csPos = csPos cs ^+^ csDir cs}


backwards :: CameraState -> CameraState
backwards cs =
  cs { csPos = csPos cs ^-^ csDir cs}


left :: CameraState -> CameraState
left cs =
    cs { csPos = csPos cs ^-^ V3 0.01 0 0}

right :: CameraState -> CameraState
right cs =
    cs { csPos = csPos cs ^+^ V3 0.01 0 0}


up :: CameraState -> CameraState
up cs =
    cs { csPos = csPos cs ^+^ V3 0 0.01 0}

down :: CameraState -> CameraState
down cs =
    cs { csPos = csPos cs ^-^ V3 0 0.01 0}


turnLeft :: CameraState -> CameraState
turnLeft cs =
  let q = axisAngle (V3 0 1 0) (0.0001 * (180 / pi))
  in
    cs { csDir = rotate q (csDir cs) }

turnRight :: CameraState -> CameraState
turnRight cs =
  let q = axisAngle (V3 0 (-1) 0) (0.0001 * (180 / pi))
  in
    cs { csDir = rotate q (csDir cs) }


vertHorizAngles :: Float -> Float -> CameraState -> CameraState
vertHorizAngles offx offy cs =
  let dir = csDir cs
      up = csUp cs
      deg = 0.0001 * (180 / pi)
      hq = axisAngle (V3 0 (-1) 0) (offx * deg)
      vq = axisAngle (V3 (-1) 0 0) (offy * deg)
      dir' = rotate vq (rotate hq dir)
      up' = rotate vq (rotate hq up)
  in
    cs { csDir = dir'
       , csUp = up' }


------
-- spherical to cartesian coords conversion
sphericalToCartesian :: Float -> Float -> V3 Float
sphericalToCartesian horang vertang =
  V3
  (cos vertang * sin horang)
  (sin vertang)
  (cos vertang * cos horang)

