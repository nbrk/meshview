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
  r1 <- lift $ randomRIO (0, 1)
  r2 <- lift $ randomRIO (0, 1)
  r3 <- lift $ randomRIO (0, 1)

  line "line-x" black (-1000000, 0, 0) (1000000, 0, 0)
  line "line-y" black (0, -1000000, 0) (0, 1000000, 0)
  line "line-z" white (0, 0, -1000000) (0, 0, 1000000)

  meshFromSTL "sample/cube.stl" "cube1" red Line
  meshFromSTL "sample/cube.stl" "cube2" lime Line
  meshFromSTL "sample/cube.stl" "cube3" blue Fill
  translateX "cube1" (-10)
  translateX "cube2" (0)
  translateX "cube3" (10)
  rotate "cube1" (0, 0, r)
  rotate "cube2" (r, r*6, 0)
  meshFromSTL "sample/monkey.stl" "monkey" white Line
  meshFromSTL "sample/rifle.stl" "rifle" magenta Fill
  meshFromSTL "sample/dragon.stl" "dragon" (RGBA 0 1 1 0) Line
  meshFromSTL "sample/tu134.stl" "tu134" yellow Line
  rotate "tu134" (90, 180, 0)
  translate "tu134" (0, -1000, (-500) - r)
  translateX "monkey" (30)
  color "monkey" (RGBA r1 r2 r3 0)
  translateX "rifle" (-100)
  translate "dragon" (60, 0, 10)
  rotate "dragon" (90, 180, 0)
  rotate "rifle" (90, 90, 0)
--  lift $ putStrLn $ "worldToRender: r " ++ show r


stepWorld :: Float -> World -> World
stepWorld nsec (World r) = World (r + 1)


main :: IO ()
main = do
--  let disp = InWindow (1024, 768) (0, 0)
  let disp = FullScreen (1024, 768)
--  let ctrl = WithoutMouse
  let ctrl = WithMouse

  simulate disp ctrl 48 (World 0) worldToRender stepWorld
--  display disp ctrl (worldToRender (World 0))



