{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Crypto.MAC.Poly1305
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides bindings to the poly1305
-- message-authentication code (MAC). The underlying implementation is
-- the @ref@ code of @poly1305@ from SUPERCOP, and should be
-- relatively fast.
--
-- For more information visit <http://cr.yp.to/mac.html>.
--
module Crypto.MAC.Poly1305
       ( Key          -- :: *
       , key          -- :: *
       , Auth(..)     -- :: *
       , authenticate -- :: ByteString -> ByteString -> Maybe Auth
       , verify       -- :: ByteString -> Auth -> ByteString -> Maybe Bool
       ) where
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           System.IO.Unsafe         (unsafePerformIO)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import           Data.ByteString.Internal (create)
import           Data.ByteString.Unsafe

data Key = Key ByteString
  deriving (Eq, Show, Ord)

key :: ByteString -> Maybe Key
key xs | S.length xs /= onetimeauthKEYBYTES = Nothing
       | otherwise = Just (Key xs)

-- | An authenticator.
data Auth = Auth { unAuth :: ByteString }
  deriving (Eq, Show, Ord)

authenticate :: Key
             -- ^ Secret key
             -> ByteString
             -- ^ Message
             -> Auth
             -- ^ Authenticator
authenticate (Key k) msg = Auth $
  unsafePerformIO . create onetimeauthBYTES $ \out ->
    unsafeUseAsCStringLen msg $ \(cstr, clen) ->
      unsafeUseAsCString k $ \pk ->
        c_crypto_onetimeauth out cstr (fromIntegral clen) pk >> return ()
{-# INLINEABLE authenticate #-}

verify :: Key
       -- ^ Secret key
       -> Auth
       -- ^ Authenticator returned via 'authenticateOnce'
       -> ByteString
       -- ^ Message
       -> Bool
       -- ^ Result: @True@ if verified, @False@ otherwise
verify (Key k) (Auth auth) msg =
  unsafePerformIO . unsafeUseAsCString auth $ \pauth ->
    unsafeUseAsCStringLen msg $ \(cstr, clen) ->
      unsafeUseAsCString k $ \pk -> do
        b <- c_crypto_onetimeauth_verify pauth cstr (fromIntegral clen) pk
        return (b == 0)
{-# INLINE verify #-}

--
-- FFI mac binding
--

onetimeauthKEYBYTES :: Int
onetimeauthKEYBYTES = 32

onetimeauthBYTES :: Int
onetimeauthBYTES = 16

foreign import ccall unsafe "poly1305_mac"
  c_crypto_onetimeauth :: Ptr Word8 -> Ptr CChar -> CULLong ->
                          Ptr CChar -> IO Int

foreign import ccall unsafe "poly1305_mac_verify"
  c_crypto_onetimeauth_verify :: Ptr CChar -> Ptr CChar -> CULLong ->
                                 Ptr CChar -> IO Int
