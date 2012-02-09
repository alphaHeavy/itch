{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Char8 ()
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr, castPtr)
import Parser (ITCHv41, parseITCHv41)

parseITCHv41' :: (Ptr CChar, Int) -> ITCHv41
parseITCHv41' (ptr, len) = parseITCHv41 (castPtr ptr) (fromIntegral len)

main :: IO ()
main = do
  -- normal success cases, using junk data
  print =<< unsafeUseAsCStringLen "E000011111111222233333333"      (return . parseITCHv41')
  print =<< unsafeUseAsCStringLen "C999988888888777766666666Y5555" (return . parseITCHv41')
  print =<< unsafeUseAsCStringLen "C999988888888777766666666N5555" (return . parseITCHv41')

  -- error cases
  -- unhandled printable flag 'Z' ------------------------- v
  print =<< unsafeUseAsCStringLen "C999988888888777766666666Z5555" (return . parseITCHv41')

  -- unknown message type -------- v
  print =<< unsafeUseAsCStringLen "Q999988888888777766666666Z5555" (return . parseITCHv41')

  -- truncated message
  print =<< unsafeUseAsCStringLen "C999988888888"                  (return . parseITCHv41')

