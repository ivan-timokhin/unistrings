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
import Data.Unistring.Singletons (Known)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Tests"
    [ testGroup
        "Memory"
        [ testGroup
            "Unsafe"
            [ testGroup "IsList" $
              let test ::
                       forall a storage alloc.
                       ( U.Primitive a
                       , Eq a
                       , Show a
                       , Arbitrary a
                       , Known storage
                       , U.Allocator storage alloc
                       )
                    => [TestTree]
                  test =
                    [ testProperty "toList . fromList == id" $ \(xs :: [a]) ->
                        let array :: U.Array alloc storage a
                            array = fromList xs
                         in toList array === xs
                    , testProperty "length . fromList == length" $ \(xs :: [a]) ->
                        let array :: U.Array alloc storage a
                            array = fromList xs
                         in U.getCountOf (U.arrayLength array) === length xs
                    ]
               in [ testGroup
                      "Native"
                      [ testGroup "Word8" (test @Word8 @'U.Native @U.Default)
                      , testGroup "Word16" (test @Word16 @'U.Native @U.Default)
                      , testGroup "Word32" (test @Word32 @'U.Native @U.Default)
                      ]
                  , testGroup
                      "Native pinned"
                      [ testGroup "Word8" (test @Word8 @'U.Native @U.Pinned)
                      , testGroup "Word16" (test @Word16 @'U.Native @U.Pinned)
                      , testGroup "Word32" (test @Word32 @'U.Native @U.Pinned)
                      ]
                  , testGroup
                      "Foreign"
                      [ testGroup "Word8" (test @Word8 @'U.Foreign @U.Pinned)
                      , testGroup "Word16" (test @Word16 @'U.Foreign @U.Pinned)
                      , testGroup "Word32" (test @Word32 @'U.Foreign @U.Pinned)
                      ]
                  ]
            ]
        ]
    ]
