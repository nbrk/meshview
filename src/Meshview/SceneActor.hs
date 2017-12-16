module Meshview.SceneActor where

import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad

import           Meshview.Message
import           Meshview.Types


actorScene :: GroupProcess Message
actorScene gref mypid = forever $
  mypid `receive`
    \msg -> case msg of
      MsgQuit -> do
        putStrLn' "actorScene: got MsgQuit, suicide"
        kill mypid
      MsgUserData r -> do
        putStrLn' "actorScene: got MsgUserData with Render"
        gref !* MsgSceneData r
      MsgRendererActive -> do
        putStrLn' "actorScene: got MsgRendererActive"
--        gref !* MsgSceneData []
      _ -> return ()


-- toInternalModel :: Model -> InternalModel
-- toInternalModel (Model n msh) = InternalModel
