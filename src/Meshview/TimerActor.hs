module Meshview.TimerActor where

import           Control.Concurrent
import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           System.Random

import           Meshview.Message
import           Meshview.Types



actorTimer :: Int -> w -> (w -> Render) -> (Float -> w -> w) -> GroupProcess Message
actorTimer hz startw wtom stepw gref mypid = do
  (subgref, spid:_) <- spawnGroup [actorTimerShooter hz startw wtom stepw]
  mergeGroups [gref, subgref]

  --addToGroup subgref
  forever $
    mypid `receive`
      \msg -> case msg of
        MsgQuit -> do
          putStrLn' "actorTimer: got MsgQuit (and kill actorTimerShooter)"
          kill spid
          kill mypid
        _                -> return ()



actorTimerShooter :: Int -> w -> (w -> Render) -> (Float -> w -> w) -> GroupProcess Message
actorTimerShooter hz startw wtom stepw gref mypid = do
  -- XXX wait for and bcast initial render (before stepw)
  let startm = wtom startw
  gref !* MsgUserData startm

  -- generate render from the world (updating it) hz times per sec
  loop 1 hz startw wtom stepw gref mypid


loop :: Int -> Int -> w -> (w -> Render) -> (Float -> w -> w) -> GroupProcess Message
loop nstep hz curw wtom stepw gref mypid = do
    threadDelay $ truncate (1000000 / fromIntegral hz)
    let neww = stepw (fromIntegral nstep / fromIntegral hz) curw
    gref !* MsgUserData (wtom neww)
    loop (nstep + 1) hz neww wtom stepw gref mypid


---
actorUntimer :: Render -> GroupProcess Message
actorUntimer r gref mypid =
  --addToGroup subgref
  forever $
    mypid `receive`
      \msg -> case msg of
        MsgQuit -> do
          putStrLn' "actorUntimer: got MsgQuit"
          kill mypid
        MsgRendererActive -> do
          putStrLn' "actorUntimer: got MsgRendererActive, bcast the Render and die"
          gref !* MsgUserData r
          kill mypid
        _                -> return ()
