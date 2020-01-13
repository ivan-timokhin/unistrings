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
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Inspection.Unistring.Memory.Unsafe
  ( tests
  ) where

import Test.Inspection ((===), hasNoType)
import Test.Tasty (TestTree)
import GHC.Exts (toList)

import qualified Data.Unistring.Memory.Unsafe as U

import Inspection.TH (inspectTest)

tests :: [TestTree]
tests =
  [ $(inspectTest "Native array length" $
      'nativeArrayLengthL === 'nativeArrayLengthR)
  , $(inspectTest "Foreign array length" $
      'foreignArrayLengthL === 'foreignArrayLengthR)
  , $(inspectTest "Native array toList no singletons" $
      'toListArrayNative `hasNoType` ''U.Sing)
  , $(inspectTest "Foreign array toList no singletons" $
      'toListArrayForeign `hasNoType` ''U.Sing)
  , $(inspectTest "Native array toList fuses" $
      'toListArrayNativeFoldr `hasNoType` ''[])
  ]

nativeArrayLengthL, nativeArrayLengthR ::
     U.Primitive a => U.Array alloc 'U.Native a -> U.CountOf a
nativeArrayLengthL = U.arrayLength
nativeArrayLengthR = U.nativeArrayLength . U.getNArray

foreignArrayLengthL, foreignArrayLengthR ::
    U.Primitive a => U.Array alloc 'U.Foreign a -> U.CountOf a
foreignArrayLengthL = U.arrayLength
foreignArrayLengthR = U.foreignArrayLength . U.getFArray

toListArrayNative ::
     (U.Allocator 'U.Native alloc, U.Primitive a)
  => U.Array alloc 'U.Native a
  -> [a]
toListArrayNative = toList

toListArrayForeign ::
     (U.Allocator 'U.Foreign alloc, U.Primitive a)
  => U.Array alloc 'U.Foreign a
  -> [a]
toListArrayForeign = toList

toListArrayNativeFoldr :: (U.Allocator 'U.Native alloc, U.Primitive a) =>
  (a -> r -> r) -> r -> U.Array alloc 'U.Native a -> r
toListArrayNativeFoldr f z = foldr f z . toList
