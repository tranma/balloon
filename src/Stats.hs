{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Applicative
import           Control.Concurrent            (getNumCapabilities)
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.ByteString.Builder.Extra
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as E
import           Options.Applicative
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude                 as PP


import           Bucket
import           Scale
import           Source

--------------------------------------------------------------------------------

type Mean  = (Int, Int)
type Coord = (Int, Int)

-- * Monoidal stats
data MStats = MStats
  { _tempMax  :: Int
  , _tempMin  :: Int
  , _tempMean :: Mean
  , _counts   :: Map Text Int
  , _firstOb  :: Maybe Coord
  , _lastOb   :: Maybe Coord
  , _distance :: Integer
  } deriving Show
makeLenses ''MStats

instance Monoid MStats where
  mempty = MStats
    minBound
    maxBound
    (0,0)
    M.empty
    Nothing
    Nothing
    0

  mappend (MStats ma1 mi1 (s1,c1) ct1 f1 l1 d1)
          (MStats ma2 mi2 (s2,c2) ct2 f2 l2 d2)
    = MStats (max ma1 ma2)
             (min mi1 mi2)
             (s1 + s2, c1 + c2)
             (M.union ct1 ct2)
             f1
             l2
             (d1 + d2 + (fromIntegral (dist l1 f2) :: Integer))

dist :: Maybe Coord -> Maybe Coord -> Int
dist (Just x) (Just y) = py x y
dist _        _        = 0

py :: Coord -> Coord -> Int
py (x1,y1) (x2,y2)
  = round (sqrt (fromIntegral ((x1 - x2) ^ (2::Int) + (y1 - y2) ^ (2::Int)) :: Double))

onlyFirst :: Coord -> Maybe Coord -> Maybe Coord
onlyFirst x Nothing    = Just x
onlyFirst _ y@(Just _) = y

mean :: Int -> Mean -> Mean
mean x (s, c) = (s + x, c + 1)

count :: (Num v, Ord k) => k -> Map k v -> Map k v
count k = M.insertWith (+) k 1


--------------------------------------------------------------------------------

producer :: MonadIO m => Bucket -> Producer Observation m ()
producer bucket = go (bGetSome bucket defaultChunkSize >-> PP.map E.decodeUtf8)
  where go p = do
          (isEnd, x, rest) <- observation p
          unless isEnd $ case x of
            Just ob -> yield ob >> go rest
            _       ->             go rest

worker :: Consumer Observation (StateT MStats IO) ()
worker = forever $ do
  x     <- await
  stats <- lift get
  let x' = x & (temp       %~ toNormal (scale (x ^. station) :: Temp))
             . (coord . _1 %~ toNormal (scale (x ^. station) :: Dist))
             . (coord . _2 %~ toNormal (scale (x ^. station) :: Dist))
  lift $ put
       $ stats & (tempMax  %~ max       (x' ^. temp))
               . (tempMin  %~ min       (x' ^. temp))
               . (tempMean %~ mean      (x' ^. temp))
               . (counts   %~ count     (x' ^. station))
               . (firstOb  %~ onlyFirst (x' ^. coord))
               . (lastOb   .~ Just      (x' ^. coord))
               . (distance +~ addDist stats x')
  where addDist stats x = fromIntegral $ dist (stats ^. lastOb) (Just (x ^. coord))

fromFile :: FilePath -> IO MStats
fromFile path = do
  caps <- getNumCapabilities
  putStrLn $ "running on " <> show caps <> " threads"

  buckets <- if caps > 1
             then splitFile caps path
             else pure <$> singleBucket path

  threads <- forM buckets $ \b ->
    async $ runEffect $ execStateP mempty $ producer b >-> worker

  stats <- mapM wait threads
  return $ mconcat stats

--------------------------------------------------------------------------------


data Opts = Opts
  { source   :: FilePath
  , useMax   :: Bool
  , useMin   :: Bool
  , useMean  :: Bool
  , useCount :: Bool
  , useDist  :: Bool }

-- could use a given accum function composed of only the stats wanted?
main :: IO ()
main = do
  Opts{..} <- execParser thing
  let pretty (MStats ma mi (s,c) ct _ _ ds)
          = mconcat
          [ "stats: " <> nl
          , if useMax   then "max(C)    = " <> show ma <> nl            else ""
          , if useMin   then "min(C)    = " <> show mi <> nl            else ""
          , if useMean  then "mean(C)   = " <> show (s `div` c) <> nl   else ""
          , if useCount then "counts    = " <> show (M.toList ct) <> nl else ""
          , if useDist  then "travelled = " <> show ds <> nl            else ""
          ]
      nl = "\n"
  stats <- fromFile source
  putStrLn $ pretty stats
  where thing = info opts (header "pick your stats")
        opts  = Opts
             <$> strOption (long "file"  <> short 'f')
             <*> switch    (long "max"   <> short 'a' )
             <*> switch    (long "min"   <> short 'i' )
             <*> switch    (long "mean"  <> short 'e' )
             <*> switch    (long "count" <> short 'c' )
             <*> switch    (long "dist"  <> short 'd' )
