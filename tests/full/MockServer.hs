{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mock Memcached server - just enough for testing client.
module MockServer
  ( MockResponse (..)
  , mockMCServer
  , withMCServer
  ) where

import Internal.Prelude

import Database.Memcache.Socket
import Database.Memcache.Types

import Blaze.ByteString.Builder
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import Control.Concurrent
import Control.Exception (SomeException, bracket, handle, throwIO)
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as N

import Database.Memcache.Errors

-- | Actions the mock server can take to a request.
data MockResponse
  = MR Response
  | CloseConnection
  | DelayMS Int MockResponse
  | Noop

-- | Run an IO action with a mock Memcached server.
--
-- This function using an 'MVar' so that the action does not run until the server is ready.
withMCServer :: Bool -> [MockResponse] -> IO () -> IO ()
withMCServer loop res action = do
  sem <- newEmptyMVar

  let waitForServer = takeMVar sem

  bracket
    (mockMCServer sem loop res)
    (\tid -> killThread tid >> threadDelay 100000)
    (\_tid -> waitForServer >> action)

-- | New mock Memcached server that responds to each request with the specified
-- list of responses.
mockMCServer :: MVar () -> Bool -> [MockResponse] -> IO ThreadId
mockMCServer sem loop resp' = forkIO
  $ bracket
    (N.socket N.AF_INET N.Stream N.defaultProtocol)
    (N.close)
  $ \sock -> do
    N.setSocketOption sock N.ReuseAddr 1
    let hints =
          N.defaultHints
            { N.addrFlags = [N.AI_PASSIVE]
            , N.addrSocketType = N.Stream
            }
    addr : _ <- N.getAddrInfo (Just hints) Nothing (Just "11211")
    N.bind sock $ N.addrAddress addr
    N.listen sock 10
    ref <- newIORef resp'

    -- signal that the server is ready
    putMVar sem ()

    acceptHandler sock ref
    when loop $ forever $ threadDelay 1000000
 where
  acceptHandler sock ref = do
    client <- fst <$> N.accept sock
    resp <- readIORef ref
    cont <- handle allErrors $ clientHandler client ref resp
    if cont
      then acceptHandler sock ref
      else return ()

  allErrors :: SomeException -> IO Bool
  allErrors = const $ return True

  clientHandler client _ [] = N.close client >> return False
  clientHandler client ref (r' : resp) = do
    void $ recvReq client
    mrHandler r'
   where
    mrHandler r = case r of
      Noop -> clientHandler client ref resp
      (MR mr) -> sendRes client mr >> clientHandler client ref resp
      (DelayMS ms mr) -> do
        writeIORef ref resp -- client may reset connection
        threadDelay (ms * 1000)
        mrHandler mr
      CloseConnection -> do
        N.close client
        writeIORef ref resp
        return $ not $ null resp

sendRes :: N.Socket -> Response -> IO ()
sendRes s m = N.sendAll s (toByteString $ szResponse m)

recvReq :: N.Socket -> IO ()
recvReq s = do
  header <- recvAll s mEMCACHE_HEADER_SIZE mempty
  let
    h = runGet (dzHeader PktRequest) (L.fromChunks [header])
    bytesToRead = fromIntegral $ bodyLen h
  when (bytesToRead > 0)
    $ void
    $ recvAll s bytesToRead mempty

recvAll :: N.Socket -> Int -> Builder -> IO B.ByteString
recvAll _ 0 !acc = return $! toByteString acc
recvAll s n !acc = do
  buf <- N.recv s n
  case B.length buf of
    0 -> throwIO errEOF
    bl
      | bl == n ->
          return $! (toByteString $! acc <> fromByteString buf)
    bl -> recvAll s (n - bl) (acc <> fromByteString buf)
 where
  errEOF :: MemcacheError
  errEOF = ProtocolError UnexpectedEOF {protocolError = ""}
