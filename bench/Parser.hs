{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark the parsing and serialization aspects of memcache.
module Main (main) where

import Internal.Prelude

import Criterion.Main
import Data.Binary.Get (runGet)
import Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Database.Memcache.Socket (dzHeader, dzResponse, szRequest)
import Database.Memcache.Types

main :: IO ()
main =
  defaultMain
    [ bgroup
        "serialize"
        [ bench "get" $ whnf szRequest' getReqMsg
        , bench "set" $ whnf szRequest' setReqMsg
        ]
    , bgroup
        "deserialize"
        [ bench "get" $ whnf dzResponse' getRespBytes
        ]
    ]

szRequest' :: Request -> L.ByteString
szRequest' = Builder.toLazyByteString . szRequest

dzResponse' :: L.ByteString -> Response
dzResponse' = dzResponse header
 where
  header = runGet (dzHeader PktResponse) getRespHeaderBytes

getReqMsg :: Request
getReqMsg =
  Req
    { reqOp = ReqGet Loud NoKey "key!"
    , reqOpaque = 123
    , reqCas = 999
    }

setReqMsg :: Request
setReqMsg =
  Req
    { reqOp = ReqSet Loud "key!" "hello world" (SESet 10 0)
    , reqOpaque = 123
    , reqCas = 999
    }

getRespHeaderBytes :: L.ByteString
getRespHeaderBytes =
  --        magic, op,               extral, 0
  L.pack
    $ [0x81, 0x00]
    ++ keyl'
    ++ [0x04, 0x00]
    ++ status'
    ++ bodyl'
    ++ opaque'
    ++ cas'
 where
  keyl' = [0x00, 0x00]
  status' = [0x00, 0x00, 0x00, 0x00]
  bodyl' = [0x00, 0x00, 0x00, 0x08]
  opaque' = [0x00, 0x00, 0x00, 0x07]
  cas' = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09]

getRespBytes :: L.ByteString
getRespBytes = getRespHeaderBytes <> extras' <> key' <> value'
 where
  extras' = L.pack [0x00, 0x00, 0x00, 0x01] -- BE: so 1?
  key' = L.pack []
  value' = LC.pack "12345678"
