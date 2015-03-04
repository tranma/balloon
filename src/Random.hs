{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiWayIf #-}

module Random
    ( WorldM, WorldS
    , Vary(..)
    , initWorld
    , sinkLines
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.State
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Monoid
import           Data.UnixTime
import           Statistics.Distribution
import           Statistics.Distribution.Normal
import qualified System.IO                      as IO
import           System.Random.MWC


type Coord = (Dim, Dim)
type Speed = Int

newtype Dim = Dim { _dim :: Int }
  deriving (Show, Eq, Ord, Real, Enum, Integral)

modDim :: Int -> Dim
modDim x = Dim (x `mod` 300)
{-# INLINE modDim #-}

instance Num Dim where
  (+) (Dim x) (Dim y) = modDim (x + y)
  (-) (Dim x) (Dim y) = modDim (x - y)
  (*) (Dim x) (Dim y) = modDim (x * y)
  abs (Dim x)         = Dim (abs x)
  signum (Dim x)      = Dim (signum x)
  fromInteger i       = modDim (fromInteger i :: Int)

makeLenses ''Dim

data Balloon = Balloon
  { _coord :: {-# UNPACK #-} !Coord
  , _temp  :: {-# UNPACK #-} !Int
  } deriving (Show)
makeLenses ''Balloon

data Scale = C | F | K
  deriving (Eq, Show)

data Station = Station
  { _location :: {-# UNPACK #-} !Coord
  , _scale    :: {-# UNPACK #-} !Scale
  , _name     :: ByteString
  } deriving (Show)
makeLenses ''Station

type Wind      = (Direction, Speed)
data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Enum, Bounded)

data World = World
  { _balloon :: {-# UNPACK #-} !Balloon
  , _posts   :: {-# UNPACK #-} ![Station]
  , _now     :: {-# UNPACK #-} !UnixTime
  , _wind    :: {-# UNPACK #-} !Wind
  } deriving (Show)
makeLenses ''World

type WorldS = (Gen (PrimState IO), World)
type WorldM = StateT WorldS IO

windDirection :: Lens' World Direction
windDirection = wind . _1

windSpeed     :: Lens' World Speed
windSpeed     = wind . _2

-- | Things that vary by a little bit, affected by something.
--
class Vary x where
  type Variant v
  vary :: (Functor m, PrimMonad m) => Gen (PrimState m) -> Variant x -> x -> m x

instance Vary Balloon where
  type Variant Balloon = (Wind, [Coord])

  vary gen ((direction, speed), stations) b = do

    -- coord
    let b' = b & (coord %~ move (direction, speed))
    if any (inRange (b' ^. coord)) stations

    -- temperature, mean current value, SD 1.0
    -- this is using a different random var to the rest to get more...randomness
    then do let t  = b' ^. temp
                tD = normalDistr (fromIntegral t) 1.0
            t'    <- round <$> genContVar tD gen
            return $ b' & (temp  .~ t')
    else    return   b'

instance Vary World where
  type Variant World = ()

  vary gen _ world = do
    -- random delta, mean 0, SD 1
    delta <- genContVar standard gen

    -- time change, mean 2, SD 10s (skewed towards progress)
    let t  = world ^. now
        td = delta * 10 + 2
        t' = addUnixDiffTime t (secondsToUnixDiffTime (round td :: Int))

    -- wind direction change, mean 0, SD 1
    let dir  = world ^. windDirection
        dir' = bound (fromEnum dir + round delta)

    -- wind speed chane, mean 0, SD 4
    let v  = world ^. windSpeed
        vd = delta * 4
        v' = v + round vd

    -- blows the balloon
    !b <- {-# SCC "balloon-vary" #-} vary gen ((dir',v'), world ^.. posts . traverse . location) (world ^. balloon)

    return $ world & (now           .~ t')
                   . (windDirection .~ dir')
                   . (windSpeed     .~ v')
                   . (balloon       .~ b)


-- | This might give more biases to the edge values.
--
bound :: Int -> Direction
bound y
  | y < fromEnum minb = minb
  | y > fromEnum maxb = maxb
  | otherwise = toEnum y
  where minb = minBound :: Direction
        maxb = maxBound :: Direction

inRange :: Coord -> Coord -> Bool
inRange (Dim a,Dim b) (Dim x,Dim y) = abs (a - x) < d && abs (b - y) < d
  where d = 50

move :: Wind -> Coord -> Coord
move (d, Dim -> s) (x,y) = case d of
  N  -> (x      , y + s)
  NE -> (x + s `div` 2, y + s `div` 2)
  E  -> (x + s  , y)
  SE -> (x + s `div` 2, y - s `div` 2)
  S  -> (x      , y - s)
  SW -> (x - s `div` 2, y - s `div` 2)
  W  -> (x - s  , y)
  NW -> (x - s `div` 2, y + s `div` 2)

convert :: Scale -> Int -> Int
convert C = id
convert F = round . (+20) . (*1.8) . (fromIntegral :: Int -> Double)
convert K = round . (+273.15)      . (fromIntegral :: Int -> Double)

sinkLines :: IO.Handle -> WorldS -> IO ()
sinkLines handle (gen, world) = mapM_ observe (world ^. posts)
  where b = world ^. balloon
        observe station
          = when ((b ^. coord) `inRange` (station ^. location)) $ do
              t <- formatUnixTime "%Y-%m-%dT%H:%M:%S" (world ^. now)
              let s = mconcat
                      [ B.byteString t
                      , B.char7 '|'
                      , B.intDec (b ^. coord . _1 . dim)
                      , B.char7 ','
                      , B.intDec (b ^. coord . _2 . dim)
                      , B.char7 '|'
                      , B.intDec (convert (station ^. scale) (b ^. temp))
                      , B.char7 '|'
                      , B.byteString (station ^. name)
                      , B.char7 '\n'
                      ]
              x <- uniformR (0,100) gen :: IO Int
              if | x < 5     -> BL.hPut       handle $ BL.drop (fromIntegral x) $ B.toLazyByteString s -- messed up
                 | otherwise -> B.hPutBuilder handle s

defaultWorld :: IO World
defaultWorld = do
  t <- getUnixTime
  return $ World
    (Balloon (0,0) 10)
    [ Station (200,-150) C "AU"
    , Station (-250,100) F "US"
    , Station (-20, 120) K "FR" ]
    t
    (S,0)

initWorld :: Gen RealWorld -> IO WorldS
initWorld gen = do
  w     <- defaultWorld
  world <- vary gen () w
  return (gen, world)