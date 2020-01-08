{-
Copyright 2019 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.UCD.Internal.Int
Description : Internal utilities for integer conversions
Copyright   : (c) Ivan Timokhin 2019
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : internal

This module has nothing to do with the main purpose of the library,
and everything to do with implementation details of encoding
enumerations in lookup tables.  There are no stability guarantees
whatsoever, and you are heavily encouraged to pretend this module
doesn't exist.
-}
module Data.Unistring.UCD.Internal.Int
  ( toInt#
  ) where

import GHC.Exts (Int#, word2Int#)
import GHC.Int (Int16(I16#), Int32(I32#), Int8(I8#))
import GHC.Word (Word16(W16#), Word8(W8#))

class ToInt# a where
  -- | Convert a lifted integer into unlifted primitive one.
  toInt# :: a -> Int#

instance ToInt# Int16 where
  toInt# (I16# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Int32 where
  toInt# (I32# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Int8 where
  toInt# (I8# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Word16 where
  toInt# (W16# w) = word2Int# w
  {-# INLINE toInt# #-}

instance ToInt# Word8 where
  toInt# (W8# w) = word2Int# w
  {-# INLINE toInt# #-}
