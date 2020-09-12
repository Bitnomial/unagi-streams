{-# LANGUAGE BangPatterns #-}

module System.IO.Streams.Concurrent.Unagi.Bounded
       ( -- * Channel conversions
         inputToChan
       , chanToInput
       , chanToOutput
       , makeChanPipe
       , chanToPipe
       , dupStream
       , splitStream
       ) where


------------------------------------------------------------------------------
import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan,
                                                        dupChan, newChan,
                                                        readChan, writeChan)
import           Control.Monad                         ((>=>))
import           Prelude                               hiding (read)
import           System.IO.Streams.Internal            (InputStream,
                                                        OutputStream,
                                                        makeInputStream,
                                                        makeOutputStream, read)


------------------------------------------------------------------------------
-- | Writes the contents of an input stream to a channel until the input stream
-- yields end-of-stream.
inputToChan :: InputStream a -> InChan (Maybe a) -> IO ()
inputToChan is ch = go
  where
    go = do
        mb <- read is
        writeChan ch mb
        maybe (return $! ()) (const go) mb


------------------------------------------------------------------------------
-- | Turns an 'OutChan' into an input stream.
--
chanToInput :: OutChan (Maybe a) -> IO (InputStream a)
chanToInput ch = makeInputStream $! readChan ch


------------------------------------------------------------------------------
-- | Turns an 'InChan' into an output stream.
--
chanToOutput :: InChan (Maybe a) -> IO (OutputStream a)
chanToOutput = makeOutputStream . writeChan


--------------------------------------------------------------------------------
-- | Create a new pair of streams using an underlying 'Chan'. Everything written
-- to the 'OutputStream' will appear as-is on the 'InputStream'.
--
-- Since reading from the 'InputStream' and writing to the 'OutputStream' are
-- blocking calls, be sure to do so in different threads.
makeChanPipe :: Int -> IO (InputStream a, OutputStream a)
makeChanPipe size = do
    (inChan, outChan) <- newChan size
    (,) <$> chanToInput outChan <*> chanToOutput inChan


--------------------------------------------------------------------------------
-- | Create a new pair of streams form the given 'Chan'. Everything written
-- to the 'OutputStream' will appear as-is on the 'InputStream'.
--
-- Since reading from the 'InputStream' and writing to the 'OutputStream' are
-- blocking calls, be sure to do so in different threads.
chanToPipe :: (InChan (Maybe a), OutChan (Maybe a)) -> IO (InputStream a, OutputStream a)
chanToPipe (inChan, outChan) = (,) <$> chanToInput outChan <*> chanToOutput inChan


--------------------------------------------------------------------------------
-- | Create a new input stream duplicated from the 'InChan'
--
dupStream :: InChan (Maybe a) -> IO (InputStream a)
dupStream = dupChan >=> chanToInput


--------------------------------------------------------------------------------
-- | Write to the output stream and have it duplicated to each input stream.
splitStream :: Int -> IO (InputStream a, InputStream a, OutputStream a)
splitStream chanSize = do
    (inChan, _) <- newChan chanSize
    in1  <- dupStream inChan
    in2  <- dupStream inChan
    out1 <- chanToOutput inChan
    return (in1, in2, out1)
