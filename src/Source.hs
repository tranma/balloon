{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Source where

import           Control.Error.Util
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Attoparsec.Text
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as E
import           GHC.IO.Handle
import           Pipes
import qualified Pipes.Attoparsec           as PA
import qualified Pipes.Parse                as PP
import           Prelude                    hiding (take, takeWhile)


data Observation = Observation
  { _time    :: {-# UNPACK #-} !Text
  , _coord   :: {-# UNPACK #-} !(Int, Int)
  , _temp    :: {-# UNPACK #-} !Int
  , _station :: {-# UNPACK #-} !Text
  } deriving (Show)

makeLenses ''Observation

-- | Parse an observation from stream of text. Signal end of input explicitly so workers can die.
--
observation
  :: Monad m
  => Producer Text m ()
  -> m (Bool, Maybe Observation, Producer Text m ())
observation stream
  =   runStateT parser stream
  >>= return . poke

  where poke ((b,m),s) = (b, join (fmap hush m), s)

        parser = do
          x <- PA.parse p
          _ <- PA.parse trim
          b <- PP.isEndOfInput
          return (b,x)
        -- seek parser to start of next line
        trim = do
          skipWhile (/= nl)
          skipWhile (== nl)
        -- parse one observation
        p = do
          s <- takeWhile (/= sep)
          _ <- skip      (== sep)
          x <- signed decimal
          _ <- skip      (== comma)
          y <- signed decimal
          _ <- skip      (== sep)
          t <- signed decimal
          _ <- skip      (== sep)
          n <- take      2
          return $ Observation s (x, y) t n
        sep    = '|'
        nl     = '\n'
        comma  = ','
{-# INLINE observation #-}

writeOb
  :: MonadIO m
  => Handle
  -> Consumer Observation m ()
writeOb handle = forever $ do
  x <- await
  liftIO $ hPutBuilder handle $ pretty x
  where pretty (Observation s (x,y) t n)
          = mconcat
            [ E.encodeUtf8Builder s
            , sep
            , intDec x
            , comma
            , intDec y
            , sep
            , intDec t
            , sep
            , E.encodeUtf8Builder n
            , nl ]
        sep   = char7 '|'
        comma = char7 ','
        nl    = char7 '\n'
