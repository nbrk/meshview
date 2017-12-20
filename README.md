# meshview

Haskell library for quick visualization and simulation of arbitrary 3D mesh data.

**The library is Work-In-Progress.**

FPS-like (WASD style) controls and camera movements are supported; a user can view/roam the dynamic 3D-scene and
explore the loaded meshes in perspective.

Only `.stl` and `.obj` files (models) are supported for now, as well as some basic affine transformations like
rotations and translations.

Objects with the same name are added (uploaded to the GPU) only once.

## Usage

### Simulation
Two basic functions are `display` and `simulate`. The engine uses `Render` to add/manipulate
objects in the scene every n-th step of the simulation (after the generation of the new `w`).
Arguments to `simulate` are:
- display and controls settings
- number of simulation steps per second
- initial `w` (a world)
- a function that specifies how a `w` is rendered (the `Render` monad)
- a function that specifies the endomorphism of `w` performed each step

Function `display` is used to draw a static scene that do not change with time.

``` haskell
import           Meshview

simulate :: Display -> Controls -> Int -> w -> (w -> Render) -> (Float -> w -> w) -> IO ()
display :: Display -> Controls -> Render -> IO ()
```

### Rendering

There are some functions to add meshes from files or vector arrays:

``` haskell
mesh :: Storable a => String -> Int -> [a] -> Color -> PrimitiveMode -> PolygonMode -> Render
meshFromSTL :: FilePath -> String -> Color -> PolygonMode -> Render
meshFromOBJ :: FilePath -> String -> Color -> PolygonMode -> Render
```

And some general transformations, applied to a named object:

``` haskell
rotateTranslateXYZ :: String -> Maybe (Float, Float, Float) -> Maybe (Float, Float, Float) -> Render
color :: String -> Color -> Render
line :: String -> Color -> (Float, Float, Float) -> (Float, Float, Float) -> Render
```

## Example

``` haskell
import           Meshview

-- custom data type
data World = World Float


main :: IO ()
main =
  -- perform simulation: generate new `Render` from current `World` 48 times per second
  simulate (FullScreen (1024, 768)) WithMouse 48 (World 0) worldToRender stepWorld


-- step the world (passed in is the current number of seconds)
stepWorld :: Float -> World -> World
stepWorld nsec (World r) = World (r + 1)


-- how to render the current state of the World
worldToRender :: World -> Render
worldToRender (World r) = do
  line "line-x" black (-1000000, 0, 0) (1000000, 0, 0)
  line "line-y" black (0, -1000000, 0) (0, 1000000, 0)
  line "line-z" white (0, 0, -1000000) (0, 0, 1000000)

  meshFromSTL "sample/cube.stl" "cube1" red Line
  meshFromSTL "sample/cube.stl" "cube2" lime Line
  meshFromOBJ "sample/house.obj" "house" blue Fill

  translateX "cube1" (-10)
  translate "house" (10, 0, 5)
  rotate "cube1" (0, 0, r)
  rotate "cube2" (r, r*6, 0)
```

## Internals
The current implementaion is an experiment in concurrent programming. The library uses simplified Actor model with messages being broadcasted to all the actors at the same time.

For example, here is the code for the `simulate` function:

``` haskell
simulate :: Display -> Controls -> Int -> w -> (w -> Render) -> (Float -> w -> w) -> IO ()
simulate disp ctrl hz startw wtom stepw = do
  runNanoErl $
    spawnGroup [
               actorCamera
               , actorScene
               , actorRenderer
               , actorGUI disp ctrl
               , actorTimer hz startw wtom stepw
               , actorTimerFPSShooter 48
               ]
```

See the included UML Sequence diagram for the details.
