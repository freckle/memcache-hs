{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Database.Memcache.Server
-- Description : Server Handling
-- Copyright   : (c) David Terei, 2016
-- License     : BSD
-- Maintainer  : code@davidterei.com
-- Stability   : stable
-- Portability : GHC
--
-- Handles the connections between a Memcached client and a single server.
--
-- Memcached expected errors (part of protocol) are returned in the Response,
-- unexpected errors (e.g., network failure) are thrown as exceptions. While
-- the Server datatype supports a `failed` and `failedAt` flag for managing
-- retries, it's up to consumers to use this.
module Database.Memcache.Server
  ( -- * Server
    Server (sid, failed)
  , newServer
  , sendRecv
  , withSocket
  , close
  ) where

import Internal.Prelude

import Control.Exception
import Data.IORef
import Data.Pool
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Database.Memcache.SASL
import Database.Memcache.Socket

import Network.Socket (HostName, ServiceName, getAddrInfo)
import qualified Network.Socket as S

-- Connection pool constants.
-- TODO: make configurable
sSTRIPES, sCONNECTIONS :: Int
sKEEPALIVE :: NominalDiffTime
sSTRIPES = 1
sCONNECTIONS = 1
sKEEPALIVE = 300

-- | Memcached server connection.
data Server = Server
  { sid :: {-# UNPACK #-} !Int
  -- ^ ID of server.
  , pool :: Pool Socket
  -- ^ Connection pool to server.
  , addr :: !HostName
  -- ^ Hostname of server.
  , port :: !ServiceName
  -- ^ Port number of server.
  , auth :: !Authentication
  -- ^ Credentials for server.
  , failed :: IORef POSIXTime
  -- ^ When did the server fail? 0 if it is alive.
  }

-- TODO:
-- weight   :: Double
-- tansport :: Transport (UDP vs. TCP)
-- poolLim  :: Int (pooled connection limit)
-- cnxnBuf   :: IORef ByteString

instance Show Server where
  show Server {..} =
    "Server [" ++ show sid ++ "] " ++ addr ++ ":" ++ show port

instance Eq Server where
  (==) x y = sid x == sid y

instance Ord Server where
  compare x y = compare (sid x) (sid y)

-- | Create a new Memcached server connection.
newServer :: Int -> HostName -> ServiceName -> Authentication -> IO Server
newServer sid host port auth = do
  fat <- newIORef 0
  pSock <-
    createPool
      connectSocket
      releaseSocket
      sSTRIPES
      sKEEPALIVE
      sCONNECTIONS
  return
    Server
      { sid = sid
      , pool = pSock
      , addr = host
      , port = port
      , auth = auth
      , failed = fat
      }
 where
  connectSocket = do
    let hints =
          S.defaultHints
            { S.addrSocketType = S.Stream
            }
    addr : _ <- getAddrInfo (Just hints) (Just host) (Just port)
    bracketOnError
      (S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr))
      releaseSocket
      ( \s -> do
          S.connect s $ S.addrAddress addr
          S.setSocketOption s S.KeepAlive 1
          S.setSocketOption s S.NoDelay 1
          authenticate s auth
          return s
      )

  releaseSocket = S.close

-- | Send and receive a single request/response pair to the Memcached server.
sendRecv :: Server -> Request -> IO Response
{-# INLINE sendRecv #-}
sendRecv svr msg = withSocket svr $ \s -> do
  send s msg
  recv s

-- | Run a function with access to an server socket for using 'send' and
-- 'recv'.
withSocket :: Server -> (Socket -> IO a) -> IO a
{-# INLINE withSocket #-}
withSocket svr = withResource $ pool svr

-- | Close the server connection. If you perform another operation after this,
-- the connection will be re-established.
close :: Server -> IO ()
{-# INLINE close #-}
close srv = destroyAllResources $ pool srv
