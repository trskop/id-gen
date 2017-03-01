{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Id.Type
    ( AnId(..)
    , Id32(..)
    , Id64(..)
    , Id128(..)

    -- * IdOffset
    , IdOffset(..)
    , IdOffset32(..)
    , IdOffset64(..)
    , IdOffset128(..)

    -- * Utilities
    , genericShowsTaggedId
    )
  where

import Prelude (Bounded, Enum, fromIntegral, maxBound, succ)

import Data.Bits (FiniteBits, finiteBitSize)
import Data.Bool (not, otherwise)
import Data.Coerce (Coercible, coerce)
import Data.Eq (Eq, (==))
import Data.Function (($), (.))
import qualified Data.List as List (null)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord, (>))
import Data.Typeable
    ( Typeable
    , showsTypeRep
    , tyConName
    , typeOf
    , typeRep
    , typeRepArgs
    , typeRepTyCon
    )
import Data.Word (Word16, Word32, Word64)
import Text.Show
    ( Show(showsPrec)
    , ShowS
    , showChar
    , showParen
    , shows
    , showString
    )

import Data.Default (Default(def))
import Data.LargeWord (Word128)


-- {{{ Id ---------------------------------------------------------------------

-- {{{ AnId -------------------------------------------------------------------

class
    ( Bounded a
    , Eq a
    , Ord a
    , Typeable a
    , IdOffset (Offset a)
    , IdOffsetNum (Offset a) ~ IdNum a
    ) => AnId a
  where
    type IdNum a :: *
    type Offset a :: *
    type IdTag a :: *

    bitSize :: a -> Word16
    default bitSize :: FiniteBits (IdNum a) => a -> Word16
    bitSize i = withIdAsNum i $ fromIntegral . finiteBitSize

    withIdAsNum :: a -> (IdNum a -> r) -> r
    default withIdAsNum :: Coercible a (IdNum a) => a -> (IdNum a -> r) -> r
    withIdAsNum i f = coerce f i

genericShowsTaggedId
    ::  ( Typeable t
        , AnId (a t)
        , Show (IdNum (a t))
        )
    => a t
    -> ShowS
genericShowsTaggedId i =
    showsType . showString " @" . showsTag . showChar ' ' . withIdAsNum i shows
      where
        showsType = showString . tyConName . typeRepTyCon $ typeOf i

        showsTag =
            showParen (not . List.null $ typeRepArgs rep) $ showsTypeRep rep
          where
            rep = typeRep i

-- }}} AnId -------------------------------------------------------------------

-- {{{ Id{32,64,128} ----------------------------------------------------------

-- | 32 bit ID space.
newtype Id32 t = Id32 Word32
  deriving (Bounded, Eq, Ord, Typeable)

-- | 64 bit ID space.
newtype Id64 t = Id64 Word64
  deriving (Bounded, Eq, Ord, Typeable)

-- | 128 bit ID space.
newtype Id128 t = Id128 Word128
  deriving (Bounded, Eq, Ord, Typeable)

instance Typeable t => Show (Id32 t) where
    showsPrec d = showParen (d > 10) . genericShowsTaggedId

instance Typeable t => Show (Id64 t) where
    showsPrec d = showParen (d > 10) . genericShowsTaggedId

instance Typeable t => Show (Id128 t) where
    showsPrec d = showParen (d > 10) . genericShowsTaggedId

instance Typeable t => AnId (Id32 t) where
    type IdNum (Id32 t) = Word32
    type Offset (Id32 t) = IdOffset32 t
    type IdTag (Id32 t) = t

instance Typeable t => AnId (Id64 t) where
    type IdNum (Id64 t) = Word64
    type Offset (Id64 t) = IdOffset64 t
    type IdTag (Id64 t) = t

instance Typeable t => AnId (Id128 t) where
    type IdNum (Id128 t) = Word128
    type Offset (Id128 t) = IdOffset128 t
    type IdTag (Id128 t) = t

-- }}} Id{32,64,128} ----------------------------------------------------------

-- }}} Id ---------------------------------------------------------------------

-- {{{ Offset -----------------------------------------------------------------

-- {{{ IdOffset ---------------------------------------------------------------

class (Bounded a, Default a, Eq a, Ord a, Typeable a) => IdOffset a where
    type IdOffsetNum a :: *

    next :: a -> Maybe a
    default next
        :: (Enum (IdOffsetNum a), Coercible a (IdOffsetNum a)) => a -> Maybe a
    next offset
      | offset == maxBound = Nothing
      | otherwise          = Just (coerce (withIdOffsetAsNum offset succ))

    withIdOffsetAsNum :: a -> (IdOffsetNum a -> r) -> r
    default withIdOffsetAsNum
        :: Coercible a (IdOffsetNum a) => a -> (IdOffsetNum a -> r)-> r
    withIdOffsetAsNum i f = f (coerce i)

-- }}} IdOffset ---------------------------------------------------------------

-- {{{ Offset{32,64,128} ------------------------------------------------------

newtype IdOffset32 t = IdOffset32 Word32
  deriving (Bounded, Eq, Ord, Show, Typeable)

newtype IdOffset64 t = IdOffset64 Word64
  deriving (Bounded, Eq, Ord, Show, Typeable)

newtype IdOffset128 t = IdOffset128 Word128
  deriving (Bounded, Eq, Ord, Show, Typeable)

instance Default (IdOffset32 t) where
    def = IdOffset32 0

instance Default (IdOffset64 t) where
    def = IdOffset64 0

instance Default (IdOffset128 t) where
    def = IdOffset128 0

instance Typeable t => IdOffset (IdOffset32 t) where
    type IdOffsetNum (IdOffset32 t) = Word32

instance Typeable t => IdOffset (IdOffset64 t) where
    type IdOffsetNum (IdOffset64 t) = Word64

instance Typeable t => IdOffset (IdOffset128 t) where
    type IdOffsetNum (IdOffset128 t) = Word128

-- }}} Offset{32,64,128} ------------------------------------------------------

-- }}} Offset -----------------------------------------------------------------
