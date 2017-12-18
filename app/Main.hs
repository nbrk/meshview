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
  line "line-x" white (-1000000, 0, 0) (1000000, 0, 0)
  line "line-y" white (0, -1000000, 0) (0, 1000000, 0)
  line "line-z" white (0, 0, -1000000) (0, 0, 1000000)

  meshFromSTL "sample/cube.stl" "cube1" red Line
  meshFromSTL "sample/cube.stl" "cube2" green Line
  meshFromSTL "sample/cube.stl" "cube3" blue Line
  translateX "cube1" (-10)
  translateX "cube2" (0)
  translateX "cube3" (10)
  rotate "cube1" (0, 0, r)
  rotate "cube2" (0, r, 0)
  rotate "cube3" (r, 0, 0)
  -- meshFromSTL "sample/cube.stl" "cube" red Line
  -- meshFromSTL "sample/monkey.stl" "monkey" green Line
  -- meshFromSTL "sample/rifle.stl" "rifle" green Line
  -- meshFromSTL "sample/dragon.stl" "dragon" green Line
  -- translateX "cube" (-10)
  -- translateX "monkey" (0)
  -- translateX "rigle" (10)
  -- translateX "dragon" (30)
--  lift $ putStrLn "worldToRender"


stepWorld :: Float -> World -> World
stepWorld nsec (World r) = World (r + 0.001)


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
