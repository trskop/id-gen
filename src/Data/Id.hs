{-# LANGUAGE NoImplicitPrelude #-}
module Data.Id
    (
    -- * Id
      AnId(..)
    , Id32
    , Id64
    , Id128

    -- * IdOffset
    , IdOffset(..)
    , IdOffset32
    , IdOffset64
    , IdOffset128

    -- * Generate
    , GenId(genId)
    )
  where

import Data.Id.Type
    ( AnId(..)
    , Id32
    , Id64
    , Id128
    , IdOffset(..)
    , IdOffset32
    , IdOffset64
    , IdOffset128
    )

import Data.Id.Gen (GenId(genId))
