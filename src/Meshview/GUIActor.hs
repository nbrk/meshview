module Meshview.GUIActor where

import           Control.Concurrent
import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import qualified Graphics.UI.GLFW                     as GLFW


import           Meshview.Message
import           Meshview.Types



-- | Init the GUI and spawn input-poll and buffer-swap actors
actorGUI :: Display -> Controls -> GroupProcess Message
actorGUI disp ctrl gref mypid = do
  w <- initWindow disp

  -- spawn local group of GUI actors
  (subgref,_) <- spawnGroup [actorGUIPoller w ctrl, actorGUISwapper w]

  -- merge with global group (let the local actors communicate with others)
  mergeGroups [gref, subgref]

  -- this actor dies, its job is done
  kill mypid


-- | Init the window and the OpenGL context
initWindow :: Display -> IO GLFW.Window
initWindow disp = do
  ok <- GLFW.init
  unless ok $
    error "Can't init GLFW"

  -- fullscreen?
  mon <- case disp of
        InWindow _ _ -> return Nothing
        FullScreen _ -> GLFW.getPrimaryMonitor

  let size = case disp of
        InWindow size _ -> size
        FullScreen size -> size

  w <- uncurry GLFW.createWindow size "viewer" mon Nothing

  when (isNothing w) $ do
    GLFW.terminate
    error "Can't create GLFW window"

  GLFW.makeContextCurrent w -- XXX

  return (fromJust w)


-- | Actor that swaps back and front buffers when the rendering is done
actorGUISwapper :: GLFW.Window -> GroupProcess Message
actorGUISwapper w gref mypid =
  forever $
    mypid `receive`
      \msg -> case msg of
        MsgQuit -> do
          putStrLn "actorGUISwapper: got MsgQuit"
          GLFW.terminate
          kill mypid
        MsgRenderingDone -> do
          putStrLn "actorGUISwapper: got MsgRenderingDone"
          GLFW.swapBuffers w
        _                -> return ()



-- | Actor that polls for input events and executes callbacks
actorGUIPoller :: GLFW.Window -> Controls -> GroupProcess Message
actorGUIPoller w ctrl gref mypid = do
  GLFW.setKeyCallback w $ Just (keyCallback gref mypid)
  GLFW.setWindowCloseCallback w $ Just (windowCloseCallback gref mypid)
  GLFW.setWindowRefreshCallback w $ Just (windowRefreshCallback gref mypid)
  when (ctrl == WithMouse) $
    GLFW.setCursorPosCallback w $ Just (cursorPosCallback gref mypid)

  -- say ok to other depending actors
  gref !* MsgGUIActive

  forever GLFW.waitEvents


keyCallback :: GroupRef Message -> Pid Message -> GLFW.KeyCallback
keyCallback gref mypid w key scancode action mods =
  -- still executing in actorGUIPoller's thread
  case key of
    GLFW.Key'W -> do
      putStrLn "keyCallback: will send MsgGUIForward"
      gref !* MsgGUIForward
    GLFW.Key'S -> do
      putStrLn "keyCallback: will send MsgGUIBackwards"
      gref !* MsgGUIBackwards
    GLFW.Key'Q -> do
      putStrLn "actorGUIPoller: broadcasting MsgQuit"
      gref !* MsgQuit
      kill mypid
    _-> putStrLn "keyCallback: noop key"


windowCloseCallback :: GroupRef Message -> Pid Message -> GLFW.WindowCloseCallback
windowCloseCallback gref mypid w = do
  -- XXX race with the final close key in wm?
  putStrLn  "windowCloseCallback: will MsgQuit and suicide"
  gref !* MsgQuit
  kill mypid


cursorPosCallback :: GroupRef Message -> Pid Message -> GLFW.CursorPosCallback
cursorPosCallback gref mypid w posx posy = do
  gref !* MsgGUIVertAngle


windowRefreshCallback :: GroupRef Message -> Pid Message -> GLFW.WindowRefreshCallback
windowRefreshCallback gref mypid w = do
  -- ask renderer to redraw when the window is damaged
  putStrLn "windowRefreshCallback"
  gref !* MsgGUIDamaged
