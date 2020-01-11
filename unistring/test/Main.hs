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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList, toList))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, (===), testProperty)

import qualified Data.Unistring.Memory.Unsafe as U

main :: IO ()
main =
  defaultMain $
  testGroup
    "Tests"
    [ testGroup
        "Memory"
        [ testGroup
            "Unsafe"
            [ testGroup
                "IsList"
                [ testGroup
                    "Native"
                    [ testGroup "Word8" (testNative @Word8 @U.Default)
                    , testGroup "Word16" (testNative @Word16 @U.Default)
                    , testGroup "Word32" (testNative @Word32 @U.Default)
                    ]
                , testGroup
                    "Native pinned"
                    [ testGroup "Word8" (testNative @Word8 @U.Pinned)
                    , testGroup "Word16" (testNative @Word16 @U.Pinned)
                    , testGroup "Word32" (testNative @Word32 @U.Pinned)
                    ]
                , testGroup
                    "Foreign"
                    [ testGroup "Word8" (testForeign @Word8 @U.Pinned)
                    , testGroup "Word16" (testForeign @Word16 @U.Pinned)
                    , testGroup "Word32" (testForeign @Word32 @U.Pinned)
                    ]
                ]
            ]
        ]
    ]

testNative ::
     forall a alloc.
     (U.Primitive a, Eq a, Show a, Arbitrary a, U.Allocator 'U.Native alloc)
  => [TestTree]
testNative =
  [ testProperty "toList . fromList == id" $ \(xs :: [a]) ->
      let array :: U.Array alloc 'U.Native a
          array = fromList xs
       in toList array === xs
  , testProperty "length . fromList == length" $ \(xs :: [a]) ->
      let array :: U.Array alloc 'U.Native a
          array = fromList xs
       in U.getCountOf (U.nativeArrayLength (U.getNArray array)) === length xs
  ]

testForeign ::
     forall a alloc.
     (U.Primitive a, Eq a, Show a, Arbitrary a, U.Allocator 'U.Foreign alloc)
  => [TestTree]
testForeign =
  [ testProperty "toList . fromList == id" $ \(xs :: [a]) ->
      let array :: U.Array alloc 'U.Foreign a
          array = fromList xs
       in toList array === xs
  , testProperty "length . fromList == length" $ \(xs :: [a]) ->
      let array :: U.Array alloc 'U.Foreign a
          array = fromList xs
       in U.getCountOf (U.foreignArrayLength (U.getFArray array)) === length xs
  ]
