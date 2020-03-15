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
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}

module Inspection.Unistring.Scalar.Sequence
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import GHC.Exts (fromListN)
import Data.Unistring.UCD (toCodePoint)

import Data.Unistring.Singletons (Known, Sing)
import Data.Unistring.Scalar.Sequence.Internal
  ( Sequence
  , Step
  , Stream
  , toList
  , equal
  )
import Data.Unistring.Memory.Primitive.Class.Unsafe (Primitive)
import Data.Unistring.Scalar.Value.Unsafe (ScalarValue(ScalarValue))
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Strictness as Strictness
import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Encoding.Form as Form

import Inspection.TH (allHaveNoneOfTypes, inspectTests)

tests :: [TestTree]
tests =
  [ testGroup
      "IsList"
      [ testGroup
          "toList + fusion"
          $(inspectTests $
            [ 'foldrNativeFullStrict32
            , 'foldrNativeFullStrict16
            , 'foldrNativeFullStrict8
            , 'foldrNativeFullLazy32
            , 'foldrNativeFullLazy16
            , 'foldrNativeFullLazy8
            , 'foldrNativeSliceStrict32
            , 'foldrNativeSliceStrict16
            , 'foldrNativeSliceStrict8
            , 'foldrNativeSliceLazy32
            , 'foldrNativeSliceLazy16
            , 'foldrNativeSliceLazy8
            , 'foldrForeignFullStrict32
            , 'foldrForeignFullStrict16
            , 'foldrForeignFullStrict8
            , 'foldrForeignFullLazy32
            , 'foldrForeignFullLazy16
            , 'foldrForeignFullLazy8
            , 'foldrForeignSliceStrict32
            , 'foldrForeignSliceStrict16
            , 'foldrForeignSliceStrict8
            , 'foldrForeignSliceLazy32
            , 'foldrForeignSliceLazy16
            , 'foldrForeignSliceLazy8
            ] `allHaveNoneOfTypes`
            [ ''[]
            , ''Stream
            , ''Step
            , ''Known
            , ''Sing
            , ''Primitive
#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 1, 0)
            , ''IO
            , ''Either
            , ''(,)
#endif
#endif
            ])
      , testGroup
          "fromListN"
          $(inspectTests $
            [ 'alphabetNativeDefaultFullStrictUTF32
            , 'alphabetNativePinnedFullStrictUTF32
            , 'alphabetForeignPinnedFullStrictUTF32
            , 'alphabetNativeDefaultFullStrictUTF16
            , 'alphabetNativePinnedFullStrictUTF16
            , 'alphabetForeignPinnedFullStrictUTF16
            , 'alphabetNativeDefaultFullStrictUTF8
            , 'alphabetNativePinnedFullStrictUTF8
            , 'alphabetForeignPinnedFullStrictUTF8
            , 'alphabetNativeDefaultFullLazyUTF32
            , 'alphabetNativePinnedFullLazyUTF32
            , 'alphabetForeignPinnedFullLazyUTF32
            , 'alphabetNativeDefaultFullLazyUTF16
            , 'alphabetNativePinnedFullLazyUTF16
            , 'alphabetForeignPinnedFullLazyUTF16
            , 'alphabetNativeDefaultFullLazyUTF8
            , 'alphabetNativePinnedFullLazyUTF8
            , 'alphabetForeignPinnedFullLazyUTF8
            , 'alphabetNativeDefaultSliceStrictUTF32
            , 'alphabetNativePinnedSliceStrictUTF32
            , 'alphabetForeignPinnedSliceStrictUTF32
            , 'alphabetNativeDefaultSliceStrictUTF16
            , 'alphabetNativePinnedSliceStrictUTF16
            , 'alphabetForeignPinnedSliceStrictUTF16
            , 'alphabetNativeDefaultSliceStrictUTF8
            , 'alphabetNativePinnedSliceStrictUTF8
            , 'alphabetForeignPinnedSliceStrictUTF8
            , 'alphabetNativeDefaultSliceLazyUTF32
            , 'alphabetNativePinnedSliceLazyUTF32
            , 'alphabetForeignPinnedSliceLazyUTF32
            , 'alphabetNativeDefaultSliceLazyUTF16
            , 'alphabetNativePinnedSliceLazyUTF16
            , 'alphabetForeignPinnedSliceLazyUTF16
            , 'alphabetNativeDefaultSliceLazyUTF8
            , 'alphabetNativePinnedSliceLazyUTF8
            , 'alphabetForeignPinnedSliceLazyUTF8
            ] `allHaveNoneOfTypes`
            [ ''Sing
            , ''Known
            , ''Stream
            , ''Step
            , ''Primitive
            , ''Allocator.Allocator
            , ''Allocator.AllocatorM
            , ''Allocator.MutableArray
            , ''IO
            ])
#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 1, 0)
      , testGroup
          "fromListN strict list fusion"
          $(inspectTests $
            [ 'alphabetNativeDefaultFullStrictUTF32
            , 'alphabetNativePinnedFullStrictUTF32
            , 'alphabetForeignPinnedFullStrictUTF32
            , 'alphabetNativeDefaultFullStrictUTF16
            , 'alphabetNativePinnedFullStrictUTF16
            , 'alphabetForeignPinnedFullStrictUTF16
            , 'alphabetNativeDefaultFullStrictUTF8
            , 'alphabetNativePinnedFullStrictUTF8
            , 'alphabetForeignPinnedFullStrictUTF8
            , 'alphabetNativeDefaultSliceStrictUTF32
            , 'alphabetNativePinnedSliceStrictUTF32
            , 'alphabetForeignPinnedSliceStrictUTF32
            , 'alphabetNativeDefaultSliceStrictUTF16
            , 'alphabetNativePinnedSliceStrictUTF16
            , 'alphabetForeignPinnedSliceStrictUTF16
            , 'alphabetNativeDefaultSliceStrictUTF8
            , 'alphabetNativePinnedSliceStrictUTF8
            , 'alphabetForeignPinnedSliceStrictUTF8
            ] `allHaveNoneOfTypes`
            [''[]])
#endif
#endif
      ]
  , testGroup
      "Eq"
      $(inspectTests $
        [ 'equal1
        , 'equal2
        , 'equal3
        , 'equal4
#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8, 2, 2, 0)
        , 'equal1E
        , 'equal2E
        , 'equal3E
#endif
#endif
        ] `allHaveNoneOfTypes`
        [ ''Sing
        , ''Known
        , ''Stream
        , ''Step
        , ''Primitive
        , ''ScalarValue
        , ''Either
        , ''(,)
        ])
  ]

foldrNativeFullStrict32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Strict 'Form.UTF32
  -> r
foldrNativeFullStrict32 f z = foldr f z . toList

foldrNativeFullStrict16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Strict 'Form.UTF16
  -> r
foldrNativeFullStrict16 f z = foldr f z . toList

foldrNativeFullStrict8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Strict 'Form.UTF8
  -> r
foldrNativeFullStrict8 f z = foldr f z . toList

foldrNativeFullLazy32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Lazy 'Form.UTF32
  -> r
foldrNativeFullLazy32 f z = foldr f z . toList

foldrNativeFullLazy16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Lazy 'Form.UTF16
  -> r
foldrNativeFullLazy16 f z = foldr f z . toList

foldrNativeFullLazy8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
  -> r
foldrNativeFullLazy8 f z = foldr f z . toList

foldrNativeSliceStrict32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict 'Form.UTF32
  -> r
foldrNativeSliceStrict32 f z = foldr f z . toList

foldrNativeSliceStrict16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict 'Form.UTF16
  -> r
foldrNativeSliceStrict16 f z = foldr f z . toList

foldrNativeSliceStrict8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
  -> r
foldrNativeSliceStrict8 f z = foldr f z . toList

foldrNativeSliceLazy32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
  -> r
foldrNativeSliceLazy32 f z = foldr f z . toList

foldrNativeSliceLazy16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
  -> r
foldrNativeSliceLazy16 f z = foldr f z . toList

foldrNativeSliceLazy8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Lazy 'Form.UTF8
  -> r
foldrNativeSliceLazy8 f z = foldr f z . toList

foldrForeignFullStrict32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Strict 'Form.UTF32
  -> r
foldrForeignFullStrict32 f z = foldr f z . toList

foldrForeignFullStrict16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Strict 'Form.UTF16
  -> r
foldrForeignFullStrict16 f z = foldr f z . toList

foldrForeignFullStrict8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Strict 'Form.UTF8
  -> r
foldrForeignFullStrict8 f z = foldr f z . toList

foldrForeignFullLazy32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Lazy 'Form.UTF32
  -> r
foldrForeignFullLazy32 f z = foldr f z . toList

foldrForeignFullLazy16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Lazy 'Form.UTF16
  -> r
foldrForeignFullLazy16 f z = foldr f z . toList

foldrForeignFullLazy8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
  -> r
foldrForeignFullLazy8 f z = foldr f z . toList

foldrForeignSliceStrict32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Strict 'Form.UTF32
  -> r
foldrForeignSliceStrict32 f z = foldr f z . toList

foldrForeignSliceStrict16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Strict 'Form.UTF16
  -> r
foldrForeignSliceStrict16 f z = foldr f z . toList

foldrForeignSliceStrict8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
  -> r
foldrForeignSliceStrict8 f z = foldr f z . toList

foldrForeignSliceLazy32 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
  -> r
foldrForeignSliceLazy32 f z = foldr f z . toList

foldrForeignSliceLazy16 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
  -> r
foldrForeignSliceLazy16 f z = foldr f z . toList

foldrForeignSliceLazy8 ::
     (ScalarValue -> r -> r)
  -> r
  -> Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Lazy 'Form.UTF8
  -> r
foldrForeignSliceLazy8 f z = foldr f z . toList

alphabetNativeDefaultFullStrictUTF32 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF32
alphabetNativeDefaultFullStrictUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedFullStrictUTF32 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Full 'Strictness.Strict 'Form.UTF32
alphabetNativePinnedFullStrictUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedFullStrictUTF32 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Full 'Strictness.Strict 'Form.UTF32
alphabetForeignPinnedFullStrictUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultFullStrictUTF16 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF16
alphabetNativeDefaultFullStrictUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedFullStrictUTF16 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Full 'Strictness.Strict 'Form.UTF16
alphabetNativePinnedFullStrictUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedFullStrictUTF16 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Full 'Strictness.Strict 'Form.UTF16
alphabetForeignPinnedFullStrictUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultFullStrictUTF8 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF8
alphabetNativeDefaultFullStrictUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedFullStrictUTF8 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Full 'Strictness.Strict 'Form.UTF8
alphabetNativePinnedFullStrictUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedFullStrictUTF8 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Full 'Strictness.Strict 'Form.UTF8
alphabetForeignPinnedFullStrictUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultFullLazyUTF32 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Lazy 'Form.UTF32
alphabetNativeDefaultFullLazyUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedFullLazyUTF32 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Full 'Strictness.Lazy 'Form.UTF32
alphabetNativePinnedFullLazyUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedFullLazyUTF32 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Full 'Strictness.Lazy 'Form.UTF32
alphabetForeignPinnedFullLazyUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultFullLazyUTF16 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Lazy 'Form.UTF16
alphabetNativeDefaultFullLazyUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedFullLazyUTF16 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Full 'Strictness.Lazy 'Form.UTF16
alphabetNativePinnedFullLazyUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedFullLazyUTF16 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Full 'Strictness.Lazy 'Form.UTF16
alphabetForeignPinnedFullLazyUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultFullLazyUTF8 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
alphabetNativeDefaultFullLazyUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedFullLazyUTF8 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
alphabetNativePinnedFullLazyUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedFullLazyUTF8 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
alphabetForeignPinnedFullLazyUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultSliceStrictUTF32 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict 'Form.UTF32
alphabetNativeDefaultSliceStrictUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedSliceStrictUTF32 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Slice 'Strictness.Strict 'Form.UTF32
alphabetNativePinnedSliceStrictUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedSliceStrictUTF32 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Slice 'Strictness.Strict 'Form.UTF32
alphabetForeignPinnedSliceStrictUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultSliceStrictUTF16 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict 'Form.UTF16
alphabetNativeDefaultSliceStrictUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedSliceStrictUTF16 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Slice 'Strictness.Strict 'Form.UTF16
alphabetNativePinnedSliceStrictUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedSliceStrictUTF16 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Slice 'Strictness.Strict 'Form.UTF16
alphabetForeignPinnedSliceStrictUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultSliceStrictUTF8 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
alphabetNativeDefaultSliceStrictUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedSliceStrictUTF8 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
alphabetNativePinnedSliceStrictUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedSliceStrictUTF8 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
alphabetForeignPinnedSliceStrictUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultSliceLazyUTF32 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
alphabetNativeDefaultSliceLazyUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedSliceLazyUTF32 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
alphabetNativePinnedSliceLazyUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedSliceLazyUTF32 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
alphabetForeignPinnedSliceLazyUTF32 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultSliceLazyUTF16 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
alphabetNativeDefaultSliceLazyUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedSliceLazyUTF16 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
alphabetNativePinnedSliceLazyUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedSliceLazyUTF16 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
alphabetForeignPinnedSliceLazyUTF16 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativeDefaultSliceLazyUTF8 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Lazy 'Form.UTF8
alphabetNativeDefaultSliceLazyUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetNativePinnedSliceLazyUTF8 ::
     Sequence 'Storage.Native Allocator.Pinned 'Ownership.Slice 'Strictness.Lazy 'Form.UTF8
alphabetNativePinnedSliceLazyUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

alphabetForeignPinnedSliceLazyUTF8 ::
     Sequence 'Storage.Foreign Allocator.Pinned 'Ownership.Slice 'Strictness.Lazy 'Form.UTF8
alphabetForeignPinnedSliceLazyUTF8 =
  fromListN 26 $ map (ScalarValue . toCodePoint) ['a' .. 'z']

equal1 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
  -> Sequence 'Storage.Foreign Allocator.Unknown 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
  -> Bool
equal1 = equal

equal2 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF32
  -> Sequence 'Storage.Native Allocator.Unknown 'Ownership.Slice 'Strictness.Strict 'Form.UTF32
  -> Bool
equal2 = equal

equal3 ::
     Sequence 'Storage.Foreign Allocator.Unknown 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
  -> Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF16
  -> Bool
equal3 = equal

equal4 ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
  -> Sequence 'Storage.Foreign Allocator.Unknown 'Ownership.Full 'Strictness.Lazy 'Form.UTF32
  -> Bool
equal4 = equal

#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8, 2, 2, 0)
equal1E ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
  -> Sequence 'Storage.Foreign Allocator.Unknown 'Ownership.Full 'Strictness.Lazy 'Form.UTF16
  -> Bool
equal1E = equal

equal2E ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF32
  -> Sequence 'Storage.Native Allocator.Unknown 'Ownership.Slice 'Strictness.Strict 'Form.UTF8
  -> Bool
equal2E = equal

equal3E ::
     Sequence 'Storage.Foreign Allocator.Unknown 'Ownership.Slice 'Strictness.Lazy 'Form.UTF16
  -> Sequence 'Storage.Native Allocator.Default 'Ownership.Full 'Strictness.Strict 'Form.UTF32
  -> Bool
equal3E = equal

_equal4E ::
     Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Lazy 'Form.UTF32
  -> Sequence 'Storage.Foreign Allocator.Unknown 'Ownership.Full 'Strictness.Lazy 'Form.UTF8
  -> Bool
_equal4E = equal
#endif
#endif
