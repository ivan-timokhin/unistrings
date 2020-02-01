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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Behaviour.Unistring.Memory.Slice
  ( tests
  ) where

import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList, toList))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, (.&&.), (===), testProperty)

import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as Primitive
import qualified Data.Unistring.Memory.Slice.Internal as Slice

import Behaviour.Common ((~/~), (~~~))
import Behaviour.Unistring.Memory.ArrayType
  ( ArrayType
  , SomeArrayType(SomeArrayType)
  )

tests :: [TestTree]
tests =
  [ testGroup "IsList" $
    let test ::
             forall a. (Primitive.Primitive a, Eq a, Show a, Arbitrary a)
          => [TestTree]
        test =
          [ testProperty "toList . fromList == id" $ \(SomeArrayType t) (xs :: [a]) ->
              let slice = fromList xs `asSliceType` t
               in toList slice === xs
          , testProperty "length . fromList == length" $ \(SomeArrayType t) (xs :: [a]) ->
              let slice = fromList xs `asSliceType` t
               in Count.getCountOf (Slice.size slice) === length xs
          ]
     in [ testGroup "Word8" (test @Word8)
        , testGroup "Word16" (test @Word16)
        , testGroup "Word32" (test @Word32)
        ]
  , testGroup "Slice unchecked" $
    let test ::
             forall a. (Primitive.Primitive a, Eq a, Show a, Arbitrary a)
          => [TestTree]
        test =
          [ testProperty "toList . slice . fromList" $
            \(SomeArrayType t)
             (prefix :: [a])
             (xs :: [a])
             (suffix :: [a]) ->
              let slice = from3Lists prefix xs suffix `asSliceType` t
               in toList slice === xs
          ]
     in [ testGroup "Word8" (test @Word8)
        , testGroup "Word16" (test @Word16)
        , testGroup "Word32" (test @Word32)
        ]
  , testGroup "Eq" $
    let test ::
             forall a. (Primitive.Primitive a, Arbitrary a, Show a, Eq a, Num a)
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "Equal" $
              \(SomeArrayType xt)
               (SomeArrayType yt)
               (prefix1 :: [a])
               (prefix2 :: [a])
               (xs :: [a])
               (suffix1 :: [a])
               (suffix2 :: [a]) ->
                let x = from3Lists prefix1 xs suffix1 `asSliceType` xt
                    y = from3Lists prefix2 xs suffix2 `asSliceType` yt
                 in x ~~~ y .&&. y ~~~ x
            , testProperty "Not equal" $
              \(SomeArrayType xt)
               (SomeArrayType yt)
               (prefix1 :: [a])
               (prefix2 :: [a])
               (xs :: [a])
               (suffix1 :: [a])
               (suffix2 :: [a]) ->
                let x = from3Lists prefix1 (xs ++ [0]) suffix1 `asSliceType` xt
                    y = from3Lists prefix2 (xs ++ [1]) suffix2 `asSliceType` yt
                 in x ~/~ y .&&. y ~/~ x
            , testProperty "Not equal length" $
              \(SomeArrayType xt)
               (SomeArrayType yt)
               (prefix1 :: [a])
               (prefix2 :: [a])
               (xs :: [a])
               (suffix1 :: [a])
               (suffix2 :: [a]) ->
                let x = from3Lists prefix1 xs suffix1 `asSliceType` xt
                    y = from3Lists prefix2 (xs ++ [1]) suffix2 `asSliceType` yt
                 in x ~/~ y .&&. y ~/~ x
            , testProperty "Random" $
              \(SomeArrayType xt)
               (SomeArrayType yt)
               (prefix1 :: [a])
               (prefix2 :: [a])
               (xs :: [a])
               (ys :: [a])
               (suffix1 :: [a])
               (suffix2 :: [a]) ->
                let x = from3Lists prefix1 xs suffix1 `asSliceType` xt
                    y = from3Lists prefix2 ys suffix2 `asSliceType` yt
                 in (x `Slice.equal` y) === (xs == ys)
                    .&&. (y `Slice.equal` x) === (ys == xs)
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  , testGroup "toArray" $
    let test ::
             forall a. (Primitive.Primitive a, Eq a, Show a, Arbitrary a)
          => String -> TestTree
        test name =
          testGroup
            name
            [ testProperty "toList match" $
              \(SomeArrayType srcT)
               (SomeArrayType destT)
               (prefix :: [a])
               (xs :: [a])
               (suffix :: [a]) ->
                let src = from3Lists prefix xs suffix `asSliceType` srcT
                    dest = Slice.toArray src `asArrayType` destT
                 in toList src === toList dest
            , testProperty "Slices match" $
              \(SomeArrayType srcT)
               (SomeArrayType destT)
               (prefix :: [a])
               (xs :: [a])
               (suffix :: [a]) ->
                let src = from3Lists prefix xs suffix `asSliceType` srcT
                    dest = Slice.fromArray (Slice.toArray src) `asSliceType` destT
                 in src ~~~ dest
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  ]

from3Lists ::
     (Primitive.Primitive a, Allocator.Allocator storage allocator)
  => [a]
  -> [a]
  -> [a]
  -> Slice.Slice storage allocator a
from3Lists prefix xs suffix =
  Slice.sliceUnchecked
    (Count.CountOf $ length prefix)
    (Count.CountOf $ length xs) $
  fromList (prefix ++ xs ++ suffix)

asSliceType ::
     Slice.Slice storage allocator a
  -> ArrayType storage allocator a
  -> Slice.Slice storage allocator a
asSliceType = const

asArrayType ::
     Array.Array storage allocator a
  -> ArrayType storage allocator a
  -> Array.Array storage allocator a
asArrayType = const
