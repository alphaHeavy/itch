{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Serialize.Get (runGet)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Parser (ITCHv41, cerealITCHv41, parseITCHv41)
import FFI
import Criterion.Main -- (Pure, bench, defaultMain, whnf)

parseITCHv41' :: (Ptr CChar, Int) -> ITCHv41
parseITCHv41' (ptr, len) = parseITCHv41 (castPtr ptr) (fromIntegral len)

bsafeLotsa :: ByteString -> IO ()
bsafeLotsa str =
  unsafeUseAsCStringLen str $ \ (ptr, len) -> do
  alloca $ \ x1 -> do
  alloca $ \ x2 -> do
  alloca $ \ x3 -> do
  alloca $ \ x4 -> do
  alloca $ \ x5 -> do
    x <- safeLotsa (castPtr ptr) (fromIntegral len) x1 x2 x3 x4 x5
    when x $ do
      _ <- peek x1
      _ <- peek x2
      _ <- peek x3
      _ <- peek x4
      _ <- peek x5
      return ()
        
bunsafeLotsa :: ByteString -> IO ()
bunsafeLotsa str =
  unsafeUseAsCStringLen str $ \ (ptr, len) -> do
  alloca $ \ x1 -> do
  alloca $ \ x2 -> do
  alloca $ \ x3 -> do
  alloca $ \ x4 -> do
  alloca $ \ x5 -> do
    x <- unsafeLotsa (castPtr ptr) (fromIntegral len) x1 x2 x3 x4 x5
    when x $ do
      _ <- peek x1
      _ <- peek x2
      _ <- peek x3
      _ <- peek x4
      _ <- peek x5
      return ()

bsafeJustOne :: ByteString -> IO ()
bsafeJustOne str =
  unsafeUseAsCStringLen str $ \ (ptr, len) -> do
  alloca $ \ x1 -> do
    x <- safeJustOne (castPtr ptr) (fromIntegral len) x1
    when x $ do
      _ <- peek x1
      return ()

bunsafeJustOne :: ByteString -> IO ()
bunsafeJustOne str =
  unsafeUseAsCStringLen str $ \ (ptr, len) -> do
  alloca $ \ x1 -> do
    x <- unsafeJustOne (castPtr ptr) (fromIntegral len) x1
    when x $ do
      _ <- peek x1
      return ()

ragel :: Benchmark
ragel = bgroup "primop"
  -- normal success cases, using junk data
  [ bench "normal1" $ unsafeUseAsCStringLen "E000011111111222233333333"      (return . parseITCHv41') >> return ()
  , bench "normal2" $ unsafeUseAsCStringLen "C999988888888777766666666Y5555" (return . parseITCHv41') >> return ()
  , bench "normal3" $ unsafeUseAsCStringLen "C999988888888777766666666N5555" (return . parseITCHv41') >> return ()

  -- error cases
  -- unhandled printable flag 'Z' ------------------------- v
  , bench "error" $ unsafeUseAsCStringLen "C999988888888777766666666Z5555"   (return . parseITCHv41') >> return ()

  -- unknown message type -------- v
  , bench "unknown" $ unsafeUseAsCStringLen "Q999988888888777766666666Z5555" (return . parseITCHv41') >> return ()

  -- truncated message
  , bench "truncated" $ unsafeUseAsCStringLen "C999988888888"                (return . parseITCHv41') >> return ()
  ]

safe :: Benchmark
safe = bgroup "safe-ffi"
  [ bench "justOne" $ bsafeJustOne "E000011111111222233333333"
  , bench "lotsa" $ bsafeLotsa "E000011111111222233333333"
  ]

unsafe :: Benchmark
unsafe = bgroup "unsafe-ffi"
  [ bench "justOne" $ bunsafeJustOne "E000011111111222233333333"
  , bench "lotsa" $ bunsafeLotsa "E000011111111222233333333"
  ]

cereal :: Benchmark
cereal = bgroup "cereal"
    -- normal success cases, using junk data
  [ bench "normal1" $ whnf (runGet cerealITCHv41) "E000011111111222233333333"
  , bench "normal2" $ whnf (runGet cerealITCHv41) "C999988888888777766666666Y5555"
  , bench "normal3" $ whnf (runGet cerealITCHv41) "C999988888888777766666666N5555"

  -- error cases
  -- unhandled printable flag 'Z' ------------------------- v
  , bench "error" $ whnf (runGet cerealITCHv41) "C999988888888777766666666Z5555"

  -- unknown message type -------- v
  , bench "unknown" $ whnf (runGet cerealITCHv41) "Q999988888888777766666666Z5555"

  -- truncated message
  , bench "truncated" $ whnf (runGet cerealITCHv41) "C999988888888"
  ]

main :: IO ()
main = defaultMain [ ragel, unsafe, safe, cereal ]

