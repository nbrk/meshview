module Main where

--import           Meshview.Display
import           Meshview.Simulate
import           Meshview.Types

--import           System.Random


data World = World Float


worldToRender :: World -> Render
worldToRender (World _) = dummyRender


stepWorld :: Float -> World -> World
stepWorld nsec (World _) = World nsec


main :: IO ()
main = do
  let disp = InWindow (640, 480) (0, 0)
  let ctrl = WithoutMouse
  let bgcol = RGBA 0.2 0.2 0.3 1

--  display disp ctrl bgcol model
  simulate disp ctrl bgcol 1 (World 100) worldToRender stepWorld


dummyRender :: Render
dummyRender = do
  return ()
