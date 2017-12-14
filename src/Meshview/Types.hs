module Meshview.Types where


--
-- Synonyms
--

-- | A type synonym for 3D point
type Point = (Float, Float, Float)


--
-- Types
--

-- | A type of 3D models (gets mapped to a list of InternalModel)
data Model =
  Model String Mesh
  | Translate Float Float Float Model
  | Color Model
  | Models [Model]



-- | A type of models' data
data Mesh =
  Points [Point]
  | Lines [Point]
  | Triangles [Point]


-- | Display configuration
data Display =
  InWindow (Int, Int) (Int, Int)
  | FullScreen (Int, Int)


-- | Controls configuration
data Controls =
  WithMouse
  | WithoutMouse
  deriving Eq


-- | Internal colors
data Color =
  RGBA Float Float Float Float
  -- | White
  -- | Black
  -- | Grey

