module Main where

-- import           Meshview.Color
-- import           Meshview.RenderMonad
-- import           Meshview.Simulate
-- import           Meshview.Types

import           Meshview
import           System.Random


data World = World Float


vs :: [Vertex3 Double]
vs =
  [ Vertex3 (-0.5) 0 (-0.5)
  , Vertex3 0 0.5 (-0.5)
  , Vertex3 0.5 0 (-0.5)
  ]

vs' :: [Vertex3 Double]
vs' =
  [ Vertex3 (-100) 0 0
  , Vertex3 100 0 0
  ]



worldToRender :: World -> Render
worldToRender (World f) = do
  r <- lift $ randomRIO (0, 1)
--  addCustomObject "tri" 3 vs red Triangles Fill
--  addCustomObject "line" 3 vs' blue Lines Fill
  addCustomObject ("cube" ++ show r) 3 (cubeHeight r) green TriangleFan Fill
  lift $ putStrLn "worldToRender"


stepWorld :: Float -> World -> World
stepWorld nsec (World i) = World nsec


main :: IO ()
main = do
  let disp = InWindow (640, 480) (0, 0)
  let ctrl = WithoutMouse

  simulate disp ctrl 1 (World 0) worldToRender stepWorld



cubeHeight :: Float -> [Vertex3 Float]
cubeHeight h =
  [
    Vertex3 (-h/2) (-h/2) (-h/2)
  , Vertex3 (-h/2) (-h/2)  (h/2)
  , Vertex3 (-h/2)  (h/2)  (h/2)
  , Vertex3 (h/2)  (h/2) (-h/2)
  , Vertex3 (-h/2) (-h/2) (-h/2)
  , Vertex3 (-h/2)  (h/2) (-h/2)
  , Vertex3 (h/2) (-h/2)  (h/2)
  , Vertex3 (-h/2) (-h/2) (-h/2)
  , Vertex3 (h/2) (-h/2) (-h/2)
  , Vertex3 (h/2)  (h/2) (-h/2)
  , Vertex3 (h/2) (-h/2) (-h/2)
  , Vertex3 (-h/2) (-h/2) (-h/2)
  , Vertex3 (-h/2) (-h/2) (-h/2)
  , Vertex3 (-h/2)  (h/2)  (h/2)
  , Vertex3 (-h/2)  (h/2) (-h/2)
  , Vertex3 (h/2) (-h/2)  (h/2)
  , Vertex3 (-h/2) (-h/2)  (h/2)
  , Vertex3 (-h/2) (-h/2) (-h/2)
  , Vertex3 (-h/2)  (h/2)  (h/2)
  , Vertex3 (-h/2) (-h/2)  (h/2)
  , Vertex3 (h/2) (-h/2)  (h/2)
  , Vertex3 (h/2)  (h/2)  (h/2)
  , Vertex3 (h/2) (-h/2) (-h/2)
  , Vertex3 (h/2)  (h/2) (-h/2)
  , Vertex3 (h/2) (-h/2) (-h/2)
  , Vertex3 (h/2)  (h/2)  (h/2)
  , Vertex3 (h/2) (-h/2)  (h/2)
  , Vertex3 (h/2)  (h/2)  (h/2)
  , Vertex3 (h/2)  (h/2) (-h/2)
  , Vertex3 (-h/2)  (h/2) (-h/2)
  , Vertex3 (h/2)  (h/2)  (h/2)
  , Vertex3 (-h/2)  (h/2) (-h/2)
  , Vertex3 (-h/2)  (h/2)  (h/2)
  , Vertex3 (h/2)  (h/2)  (h/2)
  , Vertex3 (-h/2)  (h/2)  (h/2)
  , Vertex3 (h/2) (-h/2)  (h/2)
  ]
