module Meshview.CameraActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           Linear                               hiding ((!*))

import           Meshview.Message
import           Meshview.Types


constCameraSpeed = 1


initialCameraState =
  CameraState
  { csPos = V3 0 0 3
  , csDir = V3 0 0 (-1)
  , csUp = V3 0 1 0
  , csRight = V3 1 0 0
  , csVertAngle = 0
  , csHorizAngle = pi
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
--        putStrLn $ "new CameraState: " ++ show cs'
        gref !* MsgCameraData cs'
        loop cs' gref mypid

      _ -> loop cs gref mypid


forward :: CameraState -> CameraState
forward cs =
  cs { csPos = csPos cs ^+^ (csDir cs) ^* constCameraSpeed }


backwards :: CameraState -> CameraState
backwards cs =
  cs { csPos = csPos cs ^-^ (csDir cs) ^* constCameraSpeed}


left :: CameraState -> CameraState
left cs =
    cs { csPos = csPos cs ^-^ (csRight cs) ^* constCameraSpeed}

right :: CameraState -> CameraState
right cs =
    cs { csPos = csPos cs ^+^ (csRight cs) ^* constCameraSpeed}


up :: CameraState -> CameraState
up cs =
    cs { csPos = csPos cs ^+^ (csUp cs) ^* constCameraSpeed}

down :: CameraState -> CameraState
down cs =
    cs { csPos = csPos cs ^-^ (csUp cs) ^* constCameraSpeed}


turnLeft :: CameraState -> CameraState
turnLeft cs =
  let q = axisAngle (V3 0 1 0) (1 * (pi / 180))
  in
    cs { csDir = rotate q (csDir cs) }

turnRight :: CameraState -> CameraState
turnRight cs =
  let q = axisAngle (V3 0 (-1) 0) (1 * (pi / 180))
  in
    cs { csDir = rotate q (csDir cs) }


-- XXX
-- vertHorizAngles' :: Float -> Float -> CameraState -> CameraState
-- vertHorizAngles' offx offy cs =
--   let dir = csDir cs
--       up = csUp cs
--       right = csRight cs
--       vert = csVertAngle cs + offy
--       horiz = csHorizAngle cs + offx
--       dir' = dir' ^+^ V3 (cos vert * sin horiz) (sin vert) (cos vert * cos horiz)
--       right' = right ^+^ V3 (sin (horiz - pi / 2)) 0 (cos (horiz - pi / 2))
--       up' = up ^+^ cross right' dir'
--   in
--     cs { csDir = dir'
--        , csUp = up'
--        , csRight = right'
--        , csVertAngle = fromIntegral $ (truncate vert) `mod` 360
--        , csHorizAngle = fromIntegral $ (truncate horiz) `mod` 360}


vertHorizAngles :: Float -> Float -> CameraState -> CameraState
vertHorizAngles offx offy cs =
  let dir = csDir cs
      up = csUp cs
      right = csRight cs
      rad = 1 * (pi / 180)
      xq = axisAngle (V3 (1) 0 0) (offy * rad)
      yq = axisAngle (V3 0 (1) 0) (offx * rad)
      zq = axisAngle (V3 0 0 1 :: V3 Float) (0 :: Float)
      q = yq * xq * zq
      dir' = rotate q dir
      up' = rotate q up
--      dir' = rotate zq $ rotate yq $ rotate xq dir
--      up' = (rotate xq up)
      right' = rotate yq (rotate xq right)
--      right' = cross up' dir'
  in
    cs { csDir = normalize dir'
       , csUp = normalize up'
       , csRight = normalize right' }

