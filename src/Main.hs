{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Applicative
import           Control.Concurrent         (getNumCapabilities)
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as E
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude              as PP

import           Bucket
import           Source

chunkSize :: Int
chunkSize = 1024

type Mean = (Int, Int)

-- * Monoidal stats
data MStats = MStats
  { _tempMax  :: Int
  , _tempMin  :: Int
  , _tempMean :: Mean
  , _counts   :: Map Text Int
  } deriving (Show)
makeLenses ''MStats

instance Monoid MStats where
  mempty = MStats
    minBound
    maxBound
    (0,0)
    M.empty

  mappend (MStats ma1 mi1 (s1,c1) ct1) (MStats ma2 mi2 (s2,c2) ct2)
    = MStats (max ma1 ma2)
             (min mi1 mi2)
             (s1 + s2, c1 + c2)
             (M.union ct1 ct2)

producer :: MonadIO m => Bucket -> Producer Observation m ()
producer bucket = go (bGetSome bucket chunkSize >-> PP.map E.decodeUtf8)
  where go p = do
          (flag, x, rest)   <- observation names p
          unless flag $ case x of
            Just ob -> yield ob >> go rest
            _       ->             go rest
        names = ["AU", "US", "FR"]

worker :: Consumer Observation (StateT MStats IO) ()
worker = forever $ do
  x <- await
  stats <- lift get
  lift $ put
       $ stats & (tempMax  %~ max   (x ^. temp))
               . (tempMin  %~ min   (x ^. temp))
               . (tempMean %~ mean  (x ^. temp))
               . (counts   %~ count (x ^. station))

mean :: Int -> Mean -> Mean
mean x (s, c) = (s + x, c + 1)

count :: (Num v, Ord k) => k -> Map k v -> Map k v
count k = M.insertWith (+) k 1

main :: IO ()
main = do
  caps <- getNumCapabilities
  putStrLn $ "running on " <> show caps <> " threads"

  buckets <- if caps > 1
             then splitFile caps "samples"
             else pure <$> singleBucket "samples"

  threads <- forM buckets $ \b ->
    async $ runEffect $ execStateP mempty $ producer b >-> worker

  stats <- mapM wait threads
  print $ mconcat stats
