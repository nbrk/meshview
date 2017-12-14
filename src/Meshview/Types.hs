module Meshview.Types where

import           Control.Monad.State

--
-- Synonyms
--

-- | A type synonym for 3D point
type Point = (Float, Float, Float)


--
-- Types
--

-- -- | A type of 3D models (gets mapped to a list of InternalModel)
-- data Model =
--   Model String Mesh
--   | Translate Float Float Float Model
--   | Color Model
--   | Models [Model]



-- -- | A type of models' data
-- data Mesh =
--   Points [Point]
--   | Lines [Point]
--   | Triangles [Point]


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


type Render = State RenderState ()

data RenderState = RenderState
  {
  }

-- data Scene a = Scene
--   { runScene :: a
--   }

-- instance Functor Scene where
--   fmap f (Scene a) = Scene (f a)

-- instance Applicative Scene where
--   pure = Scene
--   (<*>) (Scene fab) (Scene a) = Scene (fab a)

-- instance Monad Scene where
--   (>>=) ma (\a -> mb) =
