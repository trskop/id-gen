{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.Id.Gen
-- Description: Generate Id32, Id64, and Id128 values.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate 'Id32', 'Id64', and 'Id128' values.
module Data.Id.Gen
    (
    -- | We want to minimize the amount of information that has to be stored,
    -- but we also want to somewhat randomize the values of IDs.

    -- * GenId
      GenId(..)

    -- * Quadratic Residue
    --
    -- $quadraticResidueTheory

    -- ** Quadratic Residue for Word32
    , largestPrime32bit
    , quadraticResidue32bit

    -- ** Quadratic Residue for Word64
    , largestPrime64bit
    , quadraticResidue64bit

    -- ** Quadratic Residue for Word128
    , largestPrime128bit
    , quadraticResidue128bit

    -- ** Generic Algorithm (Unsafe)
    , quadraticResidue

    -- * Bit Operations
    --
    -- Invertible functions that operate on bits of a word.
    , swapLoAndHiHalf
    , swapEveryOtherBit
    )
  where

import Prelude (Integral, (*), (-), fromIntegral, quot, rem)

import Data.Bits (FiniteBits, (.|.), bit, finiteBitSize, xor)
import qualified Data.Bits as Bits (rotate)
import Data.Bool (otherwise)
import Data.Foldable (foldr)
import Data.Function ((.), on)
import Data.Maybe (Maybe)
import Data.Ord (Ord((<=), (>=)))
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word32, Word64)

import Data.LargeWord (Word128, Word160, Word96)

import Data.Id.Type (AnId)


-- {{{ GenId ------------------------------------------------------------------

-- | Class that describes how IDs in an ID space 'Id' are generated.
class AnId a => GenId r a | r -> a where
    -- | Generate another non-repeating ID from ID space. Value @r :: *@ is
    -- modified to ensure that the IDs do not repeate.
    --
    -- ID space can be limited, and 'Nothing' is returned when the space is
    -- exhausted. This is especially required if some kind of sharding is in
    -- place.
    genId :: r -> Maybe (a, r)


-- }}} GenId ------------------------------------------------------------------

-- {{{ Quadratic Residue ------------------------------------------------------

-- | Largest prime number that fits in to 'Word32' and is congruent to 3 modulo
-- 4.
--
-- @
-- 'largestPrime32bit' = 2^32 - 5 = 4294967291
-- @
--
-- Source:
-- <https://primes.utm.edu/lists/2small/0bit.html Primes just less than a power of two 8 to 100 bits>
largestPrime32bit :: Word32
largestPrime32bit = 4294967291

-- | Largest prime number that fits in to 'Word64' and is congruent to 3 modulo
-- 4.
--
-- @
-- 'largestPrime64bit' = 2^64 - 189 = 18446744073709551427
-- @
--
-- Source:
-- <https://primes.utm.edu/lists/2small/0bit.html Primes just less than a power of two 8 to 100 bits>
largestPrime64bit :: Word64
largestPrime64bit = 18446744073709551427

-- | Largest prime number that fits in to 'Word128' and is congruent to 3
-- modulo 4.
--
-- @
-- 'largestPrime128bit' = 2^128 - 173 = 340282366920938463463374607431768211283
-- @
--
-- Source:
-- <https://primes.utm.edu/lists/2small/100bit.html Primes just less than a power of two 101 to 200 bits>
largestPrime128bit :: Word128
largestPrime128bit = 340282366920938463463374607431768211283

-- | Generic implementation for quadratic residues, but it works correctly only
-- for non-negative integers.
--
-- __WARNING__:
--
-- * This function doesn't check if the input is non-negative.
-- * Some intermediate values overflow the original integer size. Values have
--   to be upcasted before passing them to 'quadraticResidue', and downcasted
--   afterwards. For example:
--
-- @
-- quadraticResidue32bit :: 'Word32' -> 'Word32'
-- quadraticResidue32bit =
--     'fromIntegral' . (quadraticResidue' ``on`` 'fromIntegral') 'largestPrime32bit'
--   where
--     quadraticResidue' :: 'Word64' -> 'Word64' -> 'Word64'
--     quadraticResidue' = 'quadraticResidue'
-- @
quadraticResidue :: Integral i => i -> i -> i
quadraticResidue p n
  | n >= p    = n   -- Out of range.
  | otherwise = if n <= (p `quot` 2) then q else p - q
      where
        q = (n * n) `rem` p
        -- Using quot and rem is faster than div and mod. It gives the same
        -- results for positive integers.
{-# INLINE quadraticResidue #-}

-- | Intermediate values used internally by 'quadraticResidue' may not fit in
-- to the type we want as a result.
--
-- __HELPER FUNCTION, DO NOT EXPORT__
convert :: (Integral a, Integral b) => proxy b -> (b -> b -> b) -> a -> a -> a
convert _ f x = fromIntegral . (f `on` fromIntegral) x
{-# INLINE convert #-}

-- | Quadratic residue for 32b unsigned integers that uses 'largestPrime32bit'.
quadraticResidue32bit :: Word32 -> Word32
quadraticResidue32bit =
    convert (Proxy :: Proxy Word64) quadraticResidue largestPrime32bit
    -- Maximum value used internally is n^2 for n = largestPrime32bit - 1,
    -- which doesn't fit in to 32 bits, but it does in to 64 bits.
{-# INLINE quadraticResidue32bit #-}

-- | Quadratic residue for 64b unsigned integers that uses 'largestPrime64bit'.
quadraticResidue64bit :: Word64 -> Word64
quadraticResidue64bit =
    convert (Proxy :: Proxy Word96) quadraticResidue largestPrime64bit
    -- Maximum value used internally is n^2 for n = largestPrime64bit - 1,
    -- which doesn't fit in to 64 bits, but it does in to 96 bits.
{-# INLINE quadraticResidue64bit #-}

-- | Quadratic residue for 128b unsigned integers that uses
-- 'largestPrime128bit'.
quadraticResidue128bit :: Word128 -> Word128
quadraticResidue128bit =
    convert (Proxy :: Proxy Word160) quadraticResidue largestPrime128bit
    -- Maximum value used internally is n^2 for n = largestPrime128bit - 1,
    -- which doesn't fit in to 128 bits, but it does in to 160 bits.
{-# INLINE quadraticResidue128bit #-}

-- $quadraticResidueTheory
--
-- Quadratic residue @q@ is obtained using:
--
-- @
-- x² ≡ q (mod p)
-- @
--
-- There are @(p + 1)/2@ residues (including 0) and @(p − 1)/2@ nonresidues. In
-- this case, it is customary to consider 0 as a special case and work within
-- the multiplicative group of nonzero elements of the field Z/pZ. (In other
-- words, every congruence class except zero modulo p has a multiplicative
-- inverse. This is not true for composite moduli.)
--
-- If @p ≡ 3 (mod 4)@ the negative of a residue modulo p is a nonresidue and
-- the negative of a nonresidue is a residue.
--
-- Algorithm used in here is based on the above principles and can be restated
-- as follows.
--
-- Quadratic residue @q@ is q = n² `mod` p, and it is unique as long as
-- @2n < p+1 = n < ((p+1) / 2) = n <= (p / 2)@. Numbers that can not be
-- uniquely mapped are called nonresidues, and there is as many residues as
-- there is nonresidues (when excluding 0). If @p = 3 `mod` 4@, the negative of
-- a residue modulo @p@ is a nonresidue and the negative of a nonresidue is a
-- residue. In other words if @n > (p / 2)@ then @p - (n² `mod` p)@
-- uniquely gives us the nonresidues. This way we have a bijection for numbers
-- @[1, p)@ instead of just @[1, p/2]@. To expand this to 128 bit values, we
-- map the remaining values to them selves.
--
-- Sources:
--
-- * <http://preshing.com/20121224/how-to-generate-a-sequence-of-unique-random-integers/ How to Generate a Sequence of Unique Random Integers>
--
-- * <https://en.wikipedia.org/wiki/Quadratic_residue Quadratic residue>

-- }}} Quadratic Residue ------------------------------------------------------

-- {{{ Bit Operations ---------------------------------------------------------

-- | Swap LO and HI halfs of a word.
--
-- @
-- 11110000 -> 00001111
-- @
--
-- Following holds:
--
-- @
-- 'swapLoAndHiHalf' '.' 'swapLoAndHiHalf' ≡ 'id'
-- @
swapLoAndHiHalf :: FiniteBits a => a -> a
swapLoAndHiHalf n = Bits.rotate n (finiteBitSize n `quot` 2)

-- | Swap every other bit in a word.
--
-- @
-- 00000000 -> 01010101
-- @
--
-- Following holds:
--
-- @
-- 'swapEveryOtherBit' '.' 'swapEveryOtherBit' ≡ 'id'
-- @
swapEveryOtherBit :: (Integral a, FiniteBits a) => a -> a
swapEveryOtherBit a = a `xor` mask
  where
    mask = foldr ((.|.) . bit) 0 [0, 2 .. finiteBitSize a]

-- }}} Bit Operations ---------------------------------------------------------
