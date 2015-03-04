{-# LANGUAGE ParallelListComp #-}
module Bucket where

import Data.Char
import Data.Word
import Control.Monad
import GHC.IO.Handle
import qualified Foreign.Storable as F
import qualified Foreign.Marshal.Alloc as F
import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Pipes


data Bucket = Bucket
  { bHandle :: Handle
  , begin   :: Integer
  , size    :: Maybe Integer
  } deriving Show

isEOB :: Bucket -> IO Bool
isEOB bucket = do
  eof         <- hIsEOF $ bHandle bucket
  absolutePos <- hTell  $ bHandle bucket
  when (absolutePos < begin bucket) $ error "corrupted bucket"
  let relativePos = absolutePos - begin bucket
      pastEnd     = maybe False (relativePos >=) (size bucket)
  return $ eof || pastEnd

advance :: Handle -> (Word8 -> Bool) -> IO Integer
advance h isEnd = do
  b <- F.mallocBytes 1
  go b
  F.free b
  hTell h
  where go buf = do
          c <- hGetBuf h buf 1
          unless (c == 0) $ do
            x <- F.peek buf
            unless (isEnd x) $ go buf

singleBucket :: FilePath -> IO Bucket
singleBucket path = do
  handle   <- openBinaryFile path ReadMode
  return (Bucket handle 0 Nothing)

-- thank you repa for efficient logic
-- num buckets same as requested
splitFile :: Int -> FilePath -> IO [Bucket]
splitFile num path = do
  handle   <- openBinaryFile path ReadMode
  hSeek handle SeekFromEnd 0
  fileSize <- hTell handle
  hClose handle

  let advances _      _    []             = return []
      advances _      pos1 [_h1]          = return [pos1]
      advances remain pos1 (h1 : h2 : hs) = do
            -- Push the next handle an even distance into the
            -- remaining part of the file.
            let lenWanted
                 = remain `div` fromIntegral (length (h1 : h2 : hs))
            let posWanted = pos1 + lenWanted
            hSeek h2 AbsoluteSeek posWanted

            -- Now advance it until we get to the end of a record.
            pos2          <- advance h2 (== fromIntegral (ord '\n'))
            let remain'   =  fileSize - pos2

            poss          <- advances remain' pos2 (h2 : hs)
            return $ pos1 : poss
  handles <- replicateM num (openBinaryFile path ReadMode)
  starts  <- advances fileSize 0 handles

  -- Ending positions and lengths for each bucket.
  let ends  = tail (starts ++ [fileSize])
  let lens  = map (\(start, end) -> end - start)
            $ zip starts ends

  return [ Bucket { bHandle = h
                  , begin   = start
                  , size    = Just len }
                      | start <- starts
                      | len   <- lens
                      | h     <- handles ]


bGetSome :: MonadIO m => Bucket -> Int -> Producer ByteString m ()
bGetSome bucket requested = go
  where go = do
          absolutePos <- liftIO $ hTell $ bHandle bucket
          let relativePos = absolutePos - begin bucket
              len         = case size bucket of
                Nothing -> requested
                Just s  -> min requested (fromIntegral s - fromIntegral relativePos)
          bs <- liftIO $ B.hGetSome (bHandle bucket) len
          if B.null bs
          then return ()
          else do yield bs
                  go
