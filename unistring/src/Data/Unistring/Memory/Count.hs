{-
Copyright 2020 Ivan Timokhin

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
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Count
Description : Element and byte count types
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Count
  ( CountOf(CountOf, getCountOf)
  , ByteCount(ByteCount, getByteCount)
  ) where

newtype CountOf a =
  CountOf
    { getCountOf :: Int
    }
  deriving (Eq, Ord, Show, Enum, Bounded, Num)

type role CountOf representational

newtype ByteCount =
  ByteCount
    { getByteCount :: Int
    }
  deriving (Eq, Ord, Show, Enum, Bounded, Num)
