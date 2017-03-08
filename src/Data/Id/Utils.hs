{-# LANGUAGE NoImplicitPrelude #-}
module Data.Id.Utils
    (
    -- * Word32
      toOctetsFromWord32
    , fromOctetsToWord32

    -- * Word64
    , toOctetsFromWord64
    , fromOctetsToWord64
    )
  where

import Prelude (fromIntegral)

import Data.Word (Word32, Word64, Word8)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)


toOctetsFromWord32 :: Word32 -> (Word8, Word8, Word8, Word8)
toOctetsFromWord32 x =
    ( fromIntegral ((x .&. 0xFF000000) `shiftR` 24)
    , fromIntegral ((x .&. 0x00FF0000) `shiftR` 16)
    , fromIntegral ((x .&. 0x0000FF00) `shiftR` 8 )
    , fromIntegral ( x .&. 0x000000FF             )
    )
{-# INLINE toOctetsFromWord32 #-}

toOctetsFromWord64
    :: Word64
    -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
toOctetsFromWord64 x = toOctetsFromWord64'
    (fromIntegral (x .&. 0xFFFFFFFF00000000) `shiftR` 32)
    (fromIntegral (x .&. 0x00000000FFFFFFFF)            )
  where
    toOctetsFromWord64' w1 w2 =
        joinTuples (toOctetsFromWord32 w1) (toOctetsFromWord32 w2)

    joinTuples (o1, o2, o3, o4) (o5, o6, o7, o8) =
        (o1, o2, o3, o4, o5, o6, o7, o8)
{-# INLINE toOctetsFromWord64 #-}

fromOctetsToWord32 :: (Word8, Word8, Word8, Word8) -> Word32
fromOctetsToWord32 (o1, o2, o3, o4) =
    (fromIntegral o1 `shiftL` 24) .|. (fromIntegral o2 `shiftL` 16)
    .|. (fromIntegral o3 `shiftL` 8) .|. fromIntegral o4
{-# INLINE fromOctetsToWord32 #-}

fromOctetsToWord64
    :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
    -> Word64
fromOctetsToWord64 (o1, o2, o3, o4, o5, o6, o7, o8) =
    fromOctetsToWord64' (o1, o2, o3, o4) (o5, o6, o7, o8)
  where
    fromOctetsToWord64' hi lo =
        (fromIntegral (fromOctetsToWord32 hi) `shiftL` 32)
        .|. fromIntegral (fromOctetsToWord32 lo)
{-# INLINE fromOctetsToWord64 #-}
