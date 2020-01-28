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
import Test.Tasty.QuickCheck (Arbitrary, (===), testProperty)

import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as Primitive
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage

tests :: [TestTree]
tests =
  [ testGroup "IsList" $
    let test ::
             forall a storage alloc.
             ( Primitive.Primitive a
             , Eq a
             , Show a
             , Arbitrary a
             , Allocator.Allocator storage alloc
             )
          => [TestTree]
        test =
          [ testProperty "toList . fromList == id" $ \(xs :: [a]) ->
              let slice :: Slice.Slice storage alloc a
                  slice = fromList xs
               in toList slice === xs
          , testProperty "length . fromList == length" $ \(xs :: [a]) ->
              let slice :: Slice.Slice storage alloc a
                  slice = fromList xs
               in Count.getCountOf (Slice.size slice) === length xs
          ]
     in [ testGroup
            "Native"
            [ testGroup
                "Word8"
                (test @Word8 @'Storage.Native @Allocator.Default)
            , testGroup
                "Word16"
                (test @Word16 @'Storage.Native @Allocator.Default)
            , testGroup
                "Word32"
                (test @Word32 @'Storage.Native @Allocator.Default)
            ]
        , testGroup
            "Native pinned"
            [ testGroup "Word8" (test @Word8 @'Storage.Native @Allocator.Pinned)
            , testGroup
                "Word16"
                (test @Word16 @'Storage.Native @Allocator.Pinned)
            , testGroup
                "Word32"
                (test @Word32 @'Storage.Native @Allocator.Pinned)
            ]
        , testGroup
            "Foreign"
            [ testGroup
                "Word8"
                (test @Word8 @'Storage.Foreign @Allocator.Pinned)
            , testGroup
                "Word16"
                (test @Word16 @'Storage.Foreign @Allocator.Pinned)
            , testGroup
                "Word32"
                (test @Word32 @'Storage.Foreign @Allocator.Pinned)
            ]
        ]
  , testGroup "Slice unchecked" $
    let test ::
             forall a storage alloc.
             ( Primitive.Primitive a
             , Eq a
             , Show a
             , Arbitrary a
             , Allocator.Allocator storage alloc
             )
          => [TestTree]
        test =
          [ testProperty "toList . slice . fromList" $ \(prefix :: [a]) (xs :: [a]) (suffix :: [a]) ->
              let slice :: Slice.Slice storage alloc a
                  slice = from3Lists prefix xs suffix
               in toList slice === xs
          ]
     in [ testGroup
            "Native"
            [ testGroup
                "Word8"
                (test @Word8 @'Storage.Native @Allocator.Default)
            , testGroup
                "Word16"
                (test @Word16 @'Storage.Native @Allocator.Default)
            , testGroup
                "Word32"
                (test @Word32 @'Storage.Native @Allocator.Default)
            ]
        , testGroup
            "Native pinned"
            [ testGroup "Word8" (test @Word8 @'Storage.Native @Allocator.Pinned)
            , testGroup
                "Word16"
                (test @Word16 @'Storage.Native @Allocator.Pinned)
            , testGroup
                "Word32"
                (test @Word32 @'Storage.Native @Allocator.Pinned)
            ]
        , testGroup
            "Foreign"
            [ testGroup
                "Word8"
                (test @Word8 @'Storage.Foreign @Allocator.Pinned)
            , testGroup
                "Word16"
                (test @Word16 @'Storage.Foreign @Allocator.Pinned)
            , testGroup
                "Word32"
                (test @Word32 @'Storage.Foreign @Allocator.Pinned)
            ]
        ]
  , testGroup "Eq" $
    let test ::
             forall a storage1 alloc1 storage2 alloc2.
             ( Allocator.Allocator storage1 alloc1
             , Allocator.Allocator storage2 alloc2
             , Primitive.Primitive a
             , Arbitrary a
             , Show a
             , Eq a
             , Num a
             )
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "Equal" $ \(prefix1 :: [a]) (prefix2 :: [a]) (xs :: [a]) (suffix1 :: [a]) (suffix2 :: [a]) ->
                let x :: Slice.Slice storage1 alloc1 a
                    x = from3Lists prefix1 xs suffix1
                    y :: Slice.Slice storage2 alloc2 a
                    y = from3Lists prefix2 xs suffix2
                 in x `Slice.equal` y
            , testProperty "Not equal" $ \(prefix1 :: [a]) (prefix2 :: [a]) (xs :: [a]) (suffix1 :: [a]) (suffix2 :: [a]) ->
                let x :: Slice.Slice storage1 alloc1 a
                    x = from3Lists prefix1 (xs ++ [0]) suffix1
                    y :: Slice.Slice storage2 alloc2 a
                    y = from3Lists prefix2 (xs ++ [1]) suffix2
                 in not (x `Slice.equal` y)
            , testProperty "Not equal length" $ \(prefix1 :: [a]) (prefix2 :: [a]) (xs :: [a]) (suffix1 :: [a]) (suffix2 :: [a]) ->
                let x :: Slice.Slice storage1 alloc1 a
                    x = from3Lists prefix1 xs suffix1
                    y :: Slice.Slice storage2 alloc2 a
                    y = from3Lists prefix2 (xs ++ [1]) suffix2
                 in not (x `Slice.equal` y)
            , testProperty "Random" $ \(prefix1 :: [a]) (prefix2 :: [a]) (xs :: [a]) (ys :: [a]) (suffix1 :: [a]) (suffix2 :: [a]) ->
                let x :: Slice.Slice storage1 alloc1 a
                    x = from3Lists prefix1 xs suffix1
                    y :: Slice.Slice storage2 alloc2 a
                    y = from3Lists prefix2 ys suffix2
                 in (x `Slice.equal` y) === (xs == ys)
            ]
     in [ testGroup
            "Native"
            [ test
                @Word8
                @'Storage.Native
                @Allocator.Default
                @'Storage.Native
                @Allocator.Default
                "Word8"
            , test
                @Word16
                @'Storage.Native
                @Allocator.Default
                @'Storage.Native
                @Allocator.Default
                "Word16"
            , test
                @Word32
                @'Storage.Native
                @Allocator.Default
                @'Storage.Native
                @Allocator.Default
                "Word32"
            ]
        , testGroup
            "Native pinned"
            [ test
                @Word8
                @'Storage.Native
                @Allocator.Pinned
                @'Storage.Native
                @Allocator.Pinned
                "Word8"
            , test
                @Word16
                @'Storage.Native
                @Allocator.Pinned
                @'Storage.Native
                @Allocator.Pinned
                "Word16"
            , test
                @Word32
                @'Storage.Native
                @Allocator.Pinned
                @'Storage.Native
                @Allocator.Pinned
                "Word32"
            ]
        , testGroup
            "Foreign"
            [ test
                @Word8
                @'Storage.Foreign
                @Allocator.Pinned
                @'Storage.Foreign
                @Allocator.Pinned
                "Word8"
            , test
                @Word16
                @'Storage.Foreign
                @Allocator.Pinned
                @'Storage.Foreign
                @Allocator.Pinned
                "Word16"
            , test
                @Word32
                @'Storage.Foreign
                @Allocator.Pinned
                @'Storage.Foreign
                @Allocator.Pinned
                "Word32"
            ]
        , testGroup
            "Mixed"
            [ test
                @Word8
                @'Storage.Foreign
                @Allocator.Pinned
                @'Storage.Native
                @Allocator.Default
                "Word8"
            , test
                @Word16
                @'Storage.Foreign
                @Allocator.Pinned
                @'Storage.Native
                @Allocator.Default
                "Word16"
            , test
                @Word32
                @'Storage.Foreign
                @Allocator.Pinned
                @'Storage.Native
                @Allocator.Default
                "Word32"
            ]
        ]
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
