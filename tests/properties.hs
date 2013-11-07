{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Main
       ( main  -- :: IO ()
       ) where
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S

import           Crypto.MAC.Poly1305

import           System.Environment       (getArgs)
import           Test.QuickCheck
import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Text.Printf

--------------------------------------------------------------------------------
-- Orphans

instance Arbitrary ByteString where
  arbitrary = S.pack `liftM` arbitrary

data K2 = K2 ByteString deriving Show
instance Arbitrary K2 where
  arbitrary = K2 `liftM` (arbitrary `suchThat` (\x -> S.length x == 32))

--------------------------------------------------------------------------------
-- Signatures

roundtrip :: K2 -> ByteString -> Bool
roundtrip (K2 k) xs = verify k' (authenticate k' xs) xs
  where k' = maybe (error "impossible") id (key k)

--------------------------------------------------------------------------------
-- Driver

main :: IO ()
main = do
  args <- fmap (drop 1) getArgs
  let n = if null args then 100 else read (head args) :: Int
  (results, passed) <- runTests n
  printf "Passed %d tests!\n" (sum passed)
  unless (and results) (fail "Not all tests passed!")

runTests :: Int -> IO ([Bool], [Int])
runTests ntests = fmap unzip . forM (tests ntests) $ \(s, a) ->
  printf "%-40s: " s >> a

tests :: Int -> [(String, IO (Bool,Int))]
tests ntests =
  [ ("poly1305 roundtrip", wrap roundtrip)
  ]
  where
    wrap :: Testable prop => prop -> IO (Bool, Int)
    wrap prop = do
      r <- quickCheckWithResult stdArgs{maxSize=ntests} prop
      case r of
        Success n _ _           -> return (True, n)
        GaveUp  n _ _           -> return (True, n)
#if MIN_VERSION_QuickCheck(2,6,0)
        Failure n _ _ _ _ _ _ _ -> return (False, n)
#else
        Failure n _ _ _ _ _ _   -> return (False, n)
#endif
        _                       -> return (False, 0)
