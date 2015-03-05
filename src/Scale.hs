{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Scale where

import Data.Text (Text)

data Temp = C  | F | K  deriving (Eq, Show, Read)
data Dist = KM | M | ML deriving (Eq, Show, Read)

class Normal x where
  toNormal       :: x -> Int -> Int
  fromNormal     :: x -> Int -> Int
  scale          :: Text -> x

instance Normal Temp where
  toNormal   C x                   = x
  toNormal   F (fromIntegral -> x) = round ((x - 32) / 1.8 :: Double)
  toNormal   K (fromIntegral -> x) = round (x - 273.15     :: Double)
  fromNormal C x                   = x
  fromNormal F (fromIntegral -> x) = round (x * 1.8 + 20   :: Double)
  fromNormal K (fromIntegral -> x) = round (x + 273.15     :: Double)
  scale "AU" = C
  scale "FR" = K
  scale "US" = F
  scale _    = K

instance Normal Dist where
  toNormal   KM x                   = x
  toNormal   M  (fromIntegral -> x) = round (x / 1000      :: Double)
  toNormal   ML (fromIntegral -> x) = round (x / 0.62137   :: Double)
  fromNormal KM x                   = x
  fromNormal M  (fromIntegral -> x) = round (x * 1000      :: Double)
  fromNormal ML (fromIntegral -> x) = round (x * 0.62137   :: Double)
  scale "AU" = KM
  scale "FR" = M
  scale "US" = ML
  scale _    = KM
