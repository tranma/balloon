{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.State.Strict
import           Data.ByteString.Builder.Extra
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as E
import           Options.Applicative
import           Options.Applicative.Types
import           Pipes
import qualified Pipes.ByteString              as PB
import qualified Pipes.Prelude                 as PP
import           System.IO
import           Text.Read                     hiding (lift)

import           Scale
import           Source


producer :: MonadIO m => Handle -> Producer Observation m ()
producer h = go (PB.hGetSome defaultChunkSize h >-> PP.map E.decodeUtf8)
  where go p = do
          (isEnd, x, rest) <- observation p
          unless isEnd $ case x of
            Just ob -> yield ob >> go rest
            _       ->             go rest

norm :: (Text -> Int -> Int)
     -> (Text -> Int -> Int)
     -> Observation -> Observation
norm tempF distF (Observation s (x,y) t n)
  = Observation s (distF n x, distF n y) (tempF n t) n

-- there is no way to put in parallel
normalise :: Handle -> Handle -> Maybe Temp -> Maybe Dist -> IO ()
normalise i o tscale dscale = do
  let tf = case tscale of Just t  -> \n x -> fromNormal t (toNormal (scale n :: Temp) x)
                          Nothing -> const id
  let df = case dscale of Just d  -> \n x -> fromNormal d (toNormal (scale n :: Dist) x)
                          Nothing -> const id

  runEffect $ producer i >-> PP.map (norm tf df) >-> writeOb o

--------------------------------------------------------------------------------

data Opts = Opts
  { inf       :: FilePath
  , out       :: FilePath
  , tempScale :: Maybe Temp
  , distScale :: Maybe Dist }

main :: IO ()
main = do
  Opts{..} <- execParser thing
  i        <- openFile inf ReadMode
  o        <- openFile out WriteMode
  normalise i o tempScale distScale
  hClose i
  hClose o
  where thing = info opts (header "choose your scale: temp (C,F,K), dist (KM,M,ML)")
        opts  = Opts
             <$> strOption (long "in"    <> short 'i')
             <*> strOption (long "out"   <> short 'o')
             <*> option fffuu (long "temp"  <> short 't' <> value Nothing)
             <*> option fffuu (long "dist"  <> short 'd' <> value Nothing)
        fffuu :: Read a => ReadM (Maybe a)
        fffuu = do
          x <- readerAsk
          return $ readMaybe x
