{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Parser (ITCHv41(..), parseITCHv41) where

import GHC.Int
import GHC.Prim
import GHC.Ptr
import GHC.Word

foreign import prim "ITCHv41_run"
  parseITCHv41# :: Addr# -> Word# -> (# Int#, Word#, Word#, Word#, Word#, Word# #)

data ITCHv41
  = OrderExecuted
      { nanos    :: {-# UNPACK #-} !Word64
      , refNum   :: {-# UNPACK #-} !Word64
      , shares   :: {-# UNPACK #-} !Word64
      , matchNum :: {-# UNPACK #-} !Word64 }
  | OrderExecutedWithPrice
      { nanos    :: {-# UNPACK #-} !Word64
      , refNum   :: {-# UNPACK #-} !Word64
      , shares   :: {-# UNPACK #-} !Word64
      , matchNum :: {-# UNPACK #-} !Word64
      , price    :: {-# UNPACK #-} !Word64 }
  | OtherMessage
      { status   :: {-# UNPACK #-} !Int64 }
    deriving (Show)

-- | invoke the parser primop and allocate a record with the results
parseITCHv41 :: Ptr Word8 -> Word -> ITCHv41
parseITCHv41 (Ptr buffer) (W# length) = case parseITCHv41# buffer length of
  (# 1#, nanos, shares,     _, matchNum, refNum #) ->
     OrderExecuted (W64# nanos) (W64# refNum) (W64# shares) (W64# matchNum)

  (# 2#, nanos, shares, price, matchNum, refNum #) ->
     OrderExecutedWithPrice (W64# nanos) (W64# refNum) (W64# shares) (W64# matchNum) (W64# price)

  (# status, _,      _,     _,        _,      _ #) ->
     OtherMessage  (I64# status) -- insert error handling here

