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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Unistring.Memory.Slice.Internal
  ( Slice(NativeSlice, ForeignSlice)
  ) where

import qualified Data.Unistring.Memory.Storage as Storage
import Data.Unistring.Memory.Storage (Storage)
import Data.Unistring.Memory.Count (CountOf)
import Data.Unistring.Memory.Array (Array)

data family Slice (storage :: Storage) allocator a

newtype instance Slice 'Storage.Foreign allocator a =
  ForeignSlice (Array 'Storage.Foreign allocator a)

data instance Slice 'Storage.Native allocator a =
  NativeSlice
    {-# UNPACK #-}!(Array 'Storage.Native allocator a)
    {-# UNPACK #-}!(CountOf a)
    {-# UNPACK #-}!(CountOf a)
