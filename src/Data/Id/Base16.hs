{-# LANGUAGE NoImplicitPrelude #-}
module Data.Id.Base16
    ( IdToBase16(..)
    , toBase16
    , toBase16'
    , IdFromBase16(..)
    )
  where

import Prelude (error)

import Data.Bool (not, otherwise)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord ((<))
import Data.Typeable (Typeable)

import qualified Data.ByteString.Base16.Lazy as Base16 (decode)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
    ( length
    , null
    , splitAt
    , unpack
    )
import qualified Data.ByteString.Builder as Builder
    ( toLazyByteString
    , word32HexFixed
    , word64HexFixed
    )

import Data.Id.Type (AnId, Id32(Id32), Id64(Id64), IdTag, withIdAsNum)
import Data.Id.Utils (fromOctetsToWord32, fromOctetsToWord64)


class AnId a => IdToBase16 a where
    {-# MINIMAL buildBase16' #-}

    buildBase16 :: a -> proxy (IdTag a) -> Builder
    buildBase16 a _proxy = buildBase16' a

    buildBase16' :: a -> Builder

instance Typeable t => IdToBase16 (Id32 t) where
    buildBase16' = (`withIdAsNum` Builder.word32HexFixed)

instance Typeable t => IdToBase16 (Id64 t) where
    buildBase16' = (`withIdAsNum` Builder.word64HexFixed)

toBase16 :: IdToBase16 a => a -> proxy (IdTag a) -> ByteString
toBase16 a p = Builder.toLazyByteString (buildBase16 a p)

toBase16' :: IdToBase16 a => a -> ByteString
toBase16' a = Builder.toLazyByteString (buildBase16' a)

-- TODO: instance IdToBase16 (Id128 t)

class AnId a => IdFromBase16 a where
    parseBase16 :: proxy (IdTag a) -> ByteString -> (Maybe a, ByteString)

instance Typeable t => IdFromBase16 (Id32 t) where
    parseBase16 _proxy bs
      | ByteString.length input < 8 = (Nothing, bs)
      | not (ByteString.null rest')  = (Nothing, bs)
      | otherwise = case ByteString.unpack input' of
            -- We now know that input' is exactly 4 octets long, since input is
            -- exactly 8 hexadecimal digits long. However, this only works
            -- when the input was fully decoded, which is the reason why we are
            -- checking for empty rest'.
            [o1, o2, o3, o4] ->
                (Just (Id32 (fromOctetsToWord32 (o1, o2, o3, o4))), rest)
            _ -> error "parseBase16 @(Id32 t): Imposible case."
      where
        (input, rest) = ByteString.splitAt 8 bs
        (input', rest') = Base16.decode input

instance Typeable t => IdFromBase16 (Id64 t) where
    parseBase16 _proxy bs
      | ByteString.length input < 16 = (Nothing, bs)
      | not (ByteString.null rest')  = (Nothing, bs)
      | otherwise = case ByteString.unpack input' of
            -- We now know that input' is exactly 8 octets long, since input is
            -- exactly 16 hexadecimal digits long. However, this only works
            -- only when the input was fully decoded, which is the reason why
            -- we are checking for empty rest'.
            [o1, o2, o3, o4, o5, o6, o7, o8] ->
                let n = fromOctetsToWord64 (o1, o2, o3, o4, o5, o6, o7, o8)
                in (Just (Id64 n), rest)
            _ -> error "parseBase16 @(Id64 t): Imposible case."
      where
        (input, rest) = ByteString.splitAt 16 bs
        (input', rest') = Base16.decode input
