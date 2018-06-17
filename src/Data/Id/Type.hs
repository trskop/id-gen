{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
#ifdef HAVE_TYPE_IN_TYPE
{-# LANGUAGE TypeInType #-}
#endif
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

import Prelude (Bounded, Enum, fromIntegral, maxBound, minBound, succ)

import Data.Bits (FiniteBits, finiteBitSize)
import Data.Bool (not, otherwise)
import Data.Coerce (Coercible, coerce)
import Data.Eq (Eq, (==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
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

#ifdef HAVE_TYPE_IN_TYPE
import Data.Kind (type (*))
#endif

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

#ifdef HAVE_TYPE_IN_TYPE
    type IdTag a :: k
#else
    type IdTag a :: *
#endif

    bitSize :: a -> Word16
    default bitSize :: FiniteBits (IdNum a) => a -> Word16
    bitSize i = withIdAsNum i $ fromIntegral . finiteBitSize

    idToNum :: a -> IdNum a
    default idToNum :: Coercible a (IdNum a) => a -> IdNum a
    idToNum = coerce

    withIdAsNum :: a -> (IdNum a -> r) -> r
    default withIdAsNum :: Coercible a (IdNum a) => a -> (IdNum a -> r) -> r
    withIdAsNum i f = coerce f i

    -- | Generate ID from an ID 'Offset', new 'Offset' is returned.
    genNextIdWith
        :: (IdOffsetNum (Offset a) -> IdNum a)
        -- ^ Function that generates ID value from current 'Offset'.
        -> Offset a
        -> Maybe (a, Offset a)
        -- ^ On success it returns generated ID and new 'Offset'. 'Nothing' is
        -- returned if ID space is exhausted, i.e. when @'Offset' a@ reached it's
        -- max bound.
    default genNextIdWith
        :: Coercible a (IdNum a)
        => (IdOffsetNum (Offset a) -> IdNum a)
        -> Offset a
        -> Maybe (a, Offset a)
    genNextIdWith f offset =
        (coerce (withIdOffsetAsNum offset f),) <$> next offset

-- | Show ID type using @TypeApplications@ notation. Useful for implementing
-- 'Show' instances.
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> genericShowsTaggedId (Id32 @"TheAnswer" 42) ""
-- "Id32 @\"TheAnswer\" 42"
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

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => Show (Id32 t)
  where
    showsPrec d = showParen (d > 10) . genericShowsTaggedId

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => Show (Id64 t)
  where
    showsPrec d = showParen (d > 10) . genericShowsTaggedId

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => Show (Id128 t)
  where
    showsPrec d = showParen (d > 10) . genericShowsTaggedId

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => AnId (Id32 t)
  where
    type IdNum (Id32 t) = Word32
    type Offset (Id32 t) = IdOffset32 t
    type IdTag (Id32 t) = t

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => AnId (Id64 t)
  where
    type IdNum (Id64 t) = Word64
    type Offset (Id64 t) = IdOffset64 t
    type IdTag (Id64 t) = t

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => AnId (Id128 t)
  where
    type IdNum (Id128 t) = Word128
    type Offset (Id128 t) = IdOffset128 t
    type IdTag (Id128 t) = t

-- }}} Id{32,64,128} ----------------------------------------------------------

-- }}} Id ---------------------------------------------------------------------

-- {{{ Offset -----------------------------------------------------------------

-- {{{ IdOffset ---------------------------------------------------------------

class (Bounded a, Eq a, Ord a, Typeable a) => IdOffset a where
    type IdOffsetNum a :: *

    initialValue :: a
    initialValue = minBound

    next :: a -> Maybe a
    default next
        :: (Enum (IdOffsetNum a), Coercible a (IdOffsetNum a)) => a -> Maybe a
    next offset
      | offset == maxBound = Nothing
      | otherwise          = Just (coerce (withIdOffsetAsNum offset succ))

    idOffsetToNum :: a -> IdOffsetNum a
    default idOffsetToNum :: Coercible a (IdOffsetNum a) => a -> IdOffsetNum a
    idOffsetToNum = coerce

    withIdOffsetAsNum :: a -> (IdOffsetNum a -> r) -> r
    default withIdOffsetAsNum
        :: Coercible a (IdOffsetNum a)
        => a
        -> (IdOffsetNum a -> r)
        -> r
    withIdOffsetAsNum i f = coerce f i

-- }}} IdOffset ---------------------------------------------------------------

-- {{{ Offset{32,64,128} ------------------------------------------------------

newtype IdOffset32 t = IdOffset32 Word32
  deriving (Bounded, Eq, Ord, Show, Typeable)

newtype IdOffset64 t = IdOffset64 Word64
  deriving (Bounded, Eq, Ord, Show, Typeable)

newtype IdOffset128 t = IdOffset128 Word128
  deriving (Bounded, Eq, Ord, Show, Typeable)

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => IdOffset (IdOffset32 t)
  where
    type IdOffsetNum (IdOffset32 t) = Word32

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => IdOffset (IdOffset64 t)
  where
    type IdOffsetNum (IdOffset64 t) = Word64

instance
#ifdef HAVE_TYPE_IN_TYPE
    ( Typeable k
    , Typeable (t :: k)
#else
    ( Typeable t
#endif
    )
    => IdOffset (IdOffset128 t)
  where
    type IdOffsetNum (IdOffset128 t) = Word128

-- }}} Offset{32,64,128} ------------------------------------------------------

-- }}} Offset -----------------------------------------------------------------
