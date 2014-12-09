{-# LANGUAGE BangPatterns #-}

module System.IO.Streams.Concurrent.Unagi
       ( -- * Channel conversions
           inputToChan
       , chanToInput
       , chanToOutput
       , concurrentMerge
       , makeChanPipe
       ) where


------------------------------------------------------------------------------
import           Control.Applicative           ((<$>), (<*>))
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Chan.Unagi (InChan, OutChan, newChan,
                                                readChan, writeChan)
import           Control.Concurrent.MVar       (modifyMVar, newEmptyMVar,
                                                newMVar, putMVar, takeMVar)
import           Control.Exception             (SomeException, mask, throwIO,
                                                try)
import           Control.Monad                 (forM_)
import           Prelude                       hiding (read)
import           System.IO.Streams.Internal    (InputStream, OutputStream,
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


------------------------------------------------------------------------------
-- | Concurrently merges a list of 'InputStream's, combining values in the
-- order they become available.
--
-- Note: does /not/ forward individual end-of-stream notifications, the
-- produced stream does not yield end-of-stream until all of the input streams
-- have finished.
--
-- This traps exceptions in each concurrent thread and re-raises them in the
-- current thread.
concurrentMerge :: [InputStream a] -> IO (InputStream a)
concurrentMerge iss = do
    mv    <- newEmptyMVar
    nleft <- newMVar $! length iss
    mask $ \restore -> forM_ iss $ \is -> forkIO $ do
        let producer = do
              emb <- try $ restore $ read is
              case emb of
                  Left exc      -> do putMVar mv (Left (exc :: SomeException))
                                      producer
                  Right Nothing -> putMVar mv $! Right Nothing
                  Right x       -> putMVar mv (Right x) >> producer
        producer
    makeInputStream $ chunk mv nleft

  where
    chunk mv nleft = do
        emb <- takeMVar mv
        case emb of
            Left exc      -> throwIO exc
            Right Nothing -> do x <- modifyMVar nleft $ \n ->
                                     let !n' = n - 1
                                     in return $! (n', n')
                                if x > 0
                                  then chunk mv nleft
                                  else return Nothing
            Right x       -> return x


--------------------------------------------------------------------------------
-- | Create a new pair of streams using an underlying 'Chan'. Everything written
-- to the 'OutputStream' will appear as-is on the 'InputStream'.
--
-- Since reading from the 'InputStream' and writing to the 'OutputStream' are
-- blocking calls, be sure to do so in different threads.
makeChanPipe :: IO (InputStream a, OutputStream a)
makeChanPipe = do
    (inChan, outChan) <- newChan
    (,) <$> chanToInput outChan <*> chanToOutput inChan
