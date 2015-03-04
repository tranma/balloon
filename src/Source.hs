{-# LANGUAGE TemplateHaskell #-}

module Source where

import           Control.Lens
import Data.Attoparsec.Text
import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Parse as PP
import Control.Monad.State.Strict
import Control.Error.Util
import Data.Text (Text)

data Observation = Observation
  { _coord   :: {-# UNPACK #-} !(Int, Int)
  , _temp    :: {-# UNPACK #-} !Int
  , _station :: {-# UNPACK #-} !Text
  } deriving (Show)

makeLenses ''Observation

-- | Parse an observation from stream of text. Signal end of input explicitly so workers can die.
--
observation :: Monad m => [Text] -> Producer Text m () -> m (Bool, Maybe Observation, Producer Text m ())
observation names stream
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
          _ <- skipWhile (/= sep)
          _ <- skip      (== sep)
          x <- signed decimal
          _ <- skip      (== comma)
          y <- signed decimal
          _ <- skip      (== sep)
          t <- signed decimal
          _ <- skip      (== sep)
          n <- choice    (map string names)
          return $ Observation (x,y) t n
        sep    = '|'
        nl     = '\n'
        comma  = ','
{-# INLINE observation #-}
