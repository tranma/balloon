{-# LANGUAGE TypeFamilies #-}

-- trust me it's fine
{-# LANGUAGE UndecidableInstances #-}

import Data.Text (Text)
import System.Random.MWC
import Control.Monad.State
import Control.Monad.Primitive
import Data.Time.Clock

type Coord = (Int, Int)
type ID    = Text
type Speed = Int
type Post  = (ID, Coord)

type Wind = (Direction, Speed)

data World = World
  { _balloon :: Balloon
  , _posts   :: [Post]
  , _now     :: UTCTime
  , _wind    :: Wind }

data Balloon = Balloon 
  { _location  :: Coord
  , _temp      :: Int
  } 

data Direction = N | NE | E | SE | S | SW | W | NW

type Ob = (UTCTime, Coord, Int)

type WorldS = (Gen (PrimState IO), World)
type WorldM = StateT WorldS IO

-- | Things that vary by a little bit, affected by something.
--
class Vary x where
  type Variant v
  vary :: v -> x -> m x

instance Vary Balloon where
  type Variant Balloon = Wind
  vary (direction, speed) balloon = _

instance Vary World where
  type Variant World = Gen (PrimState IO)
  vary gen world = _

init :: IO WorldS
init = do
  gen <- create
  return (gen, _)

main = undefined
