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
worldToRender (World r) = do
--  r <- lift $ randomRIO (0, 1)
--  addCustomObject "tri" 3 vs red Triangles Fill
  mesh "cube" 3 (cubeHeight 1) red TriangleFan Fill
  translateX "cube" (r * 100)
  mesh "line" 3 vs' blue Lines Fill
  rotateX "line" (-r)
--  lift $ putStrLn "worldToRender"


stepWorld :: Float -> World -> World
stepWorld nsec (World r) = World (r + 0.0001)


main :: IO ()
main = do
--  let disp = InWindow (1024, 768) (0, 0)
  let disp = FullScreen (1024, 768)
--  let ctrl = WithoutMouse
  let ctrl = WithMouse

  simulate disp ctrl 48 (World 0) worldToRender stepWorld
--  display disp ctrl (worldToRender (World 0))



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
