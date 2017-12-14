module Meshview.Timer where

import           Control.Concurrent
import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           System.Random

import           Meshview.Message
import           Meshview.Types



actorTimer :: Int -> w -> (w -> Model) -> (Float -> w -> w) -> GroupProcess Message
actorTimer hz startw wtom stepw gref mypid = do
  (subgref, spid:_) <- spawnGroup [actorTimerShooter hz startw wtom stepw]
  mergeGroups [gref, subgref]

  --addToGroup subgref
  forever $
    mypid `receive`
      \msg -> case msg of
        MsgQuit -> do
          putStrLn "actorTimer: got MsgQuit (and kill actorTimerShooter)"
          kill spid
          kill mypid
        _                -> return ()



actorTimerShooter :: Int -> w -> (w -> Model) -> (Float -> w -> w) -> GroupProcess Message
actorTimerShooter hz startw wtom stepw gref mypid = do
  -- bcast initial model (before stepw)
  let startm = wtom startw
  gref !* MsgUserData startm

  loop 1 hz startw wtom stepw gref mypid


loop :: Int -> Int -> w -> (w -> Model) -> (Float -> w -> w) -> GroupProcess Message
loop nstep hz curw wtom stepw gref mypid = do
    threadDelay $ truncate (1000000 / fromIntegral hz)
    let neww = stepw (fromIntegral nstep / fromIntegral hz) curw
    gref !* MsgUserData (wtom neww)
    loop (nstep + 1) hz neww wtom stepw gref mypid
