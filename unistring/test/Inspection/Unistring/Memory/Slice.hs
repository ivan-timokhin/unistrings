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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Inspection.Unistring.Memory.Slice
  ( tests
  ) where

import GHC.Exts (ByteArray#, Int(I#), Int#)
import Test.Inspection (hasNoType)
import Test.Tasty (TestTree, testGroup)

import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage

import Inspection.TH (inspectTest)

tests :: [TestTree]
tests =
  [ testGroup
      "Unpack"
      [ testGroup
          "Native"
          [ $(inspectTest "NativeArray" $
              'mkNativeSlice `hasNoType` ''Array.NativeArray)
          , $(inspectTest "Array" $ 'mkNativeSlice `hasNoType` ''Array.Array)
          , $(inspectTest "CountOf" $ 'mkNativeSlice `hasNoType` ''Count.CountOf)
          , $(inspectTest "Int" $ 'mkNativeSlice `hasNoType` ''Int)
          ]
      ]
  ]

mkNativeSlice ::
     ByteArray# -> Int# -> Int# -> Slice.Slice 'Storage.Native allocator a
mkNativeSlice ba# offset# len# =
  Slice.NativeSlice
    (Array.NArray (Array.NativeArray ba#))
    (Count.CountOf (I# offset#))
    (Count.CountOf (I# len#))
