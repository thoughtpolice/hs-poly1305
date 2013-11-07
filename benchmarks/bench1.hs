{-# LANGUAGE CPP #-}
module Main
       ( main -- :: IO ()
       ) where

import Criterion.Main
import Crypto.MAC.Poly1305

import Control.DeepSeq
import qualified Data.ByteString as B

--------------------------------------------------------------------------------

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString
#endif

instance NFData Auth where
  rnf (Auth x) = rnf x

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let dummy = B.pack [1..512]
      k     = maybe (error "impossible") id (key $ B.pack [0..31])
      msg   = authenticate k dummy
  defaultMain
    [ bench "authenticate" $ nf (authenticate k) dummy
    , bench "verify"       $ nf (verify k)       msg
    , bench "roundtrip"    $ nf (roundtrip k)    dummy
    ]

roundtrip :: Key -> B.ByteString -> Bool
roundtrip k xs = verify k (authenticate k xs) xs
