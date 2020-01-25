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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Behaviour.Unistring.Memory.Unsafe
  ( tests
  ) where

import Data.Functor.Classes (Eq1(liftEq))
import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList, toList))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, (===), testProperty)

import qualified Data.Unistring.Memory.Unsafe as U

tests :: [TestTree]
tests =
  [ testGroup "IsList" $
    let test ::
             forall a storage alloc.
             ( U.Primitive a
             , Eq a
             , Show a
             , Arbitrary a
             , U.Allocator storage alloc
             )
          => [TestTree]
        test =
          [ testProperty "toList . fromList == id" $ \(xs :: [a]) ->
              let array :: U.Array storage alloc a
                  array = fromList xs
               in toList array === xs
          , testProperty "length . fromList == length" $ \(xs :: [a]) ->
              let array :: U.Array storage alloc a
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
  , testGroup "Adoption" $
    let testSuccess ::
             forall storage1 alloc1 storage2 alloc2 a.
             ( U.Allocator storage1 alloc1
             , U.Allocator storage2 alloc2
             , U.Primitive a
             , Arbitrary a
             , Show a
             )
          => String
          -> TestTree
        testSuccess name =
          testProperty name $ \(xs :: [a]) ->
            let array :: U.Array storage1 alloc1 a
                array = fromList xs
                adopted :: Maybe (U.Array storage2 alloc2 a)
                adopted = U.adopt array
             in liftEq U.arrayEq adopted (Just array)
     in [ testGroup "Self" $
          let test ::
                   forall storage alloc a.
                   ( U.Allocator storage alloc
                   , U.Primitive a
                   , Arbitrary a
                   , Show a
                   )
                => String
                -> TestTree
              test = testSuccess @storage @alloc @storage @alloc @a
           in [ test @'U.Native @U.Default @Word8 "Native Default"
              , test @'U.Native @U.Pinned @Word8 "Native Pinned"
              , test @'U.Foreign @U.Pinned @Word8 "Foreign Pinned"
              ]
        , testSuccess
            @'U.Native
            @U.Pinned
            @'U.Foreign
            @U.Pinned
            @Word8
            "Native/Pinned -> Foreign/Pinned"
        ]
  , testGroup "Array Eq" $
    let test ::
             forall a storage alloc.
             ( U.Allocator storage alloc
             , U.Primitive a
             , Num a
             , Arbitrary a
             , Show a
             , Eq a
             )
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "Equal" $ \(xs :: [a]) ->
                let x, y :: U.Array storage alloc a
                    x = fromList xs
                    y = fromList xs
                 in x == y
            , testProperty "Not equal" $ \(xs :: [a]) ->
                let x, y :: U.Array storage alloc a
                    x = fromList (xs ++ [0])
                    y = fromList (xs ++ [1])
                 in x /= y
            , testProperty "Not equal length" $ \(xs :: [a]) ->
                let x, y :: U.Array storage alloc a
                    x = fromList xs
                    y = fromList (xs ++ [1])
                 in x /= y && y /= x
            , testProperty "Random" $ \xs ys ->
                let x, y :: U.Array storage alloc a
                    x = fromList xs
                    y = fromList ys
                 in (x == y) == (xs == ys)
            ]
     in [ testGroup
            "Native"
            [ test @Word8 @'U.Native @U.Default "Word8"
            , test @Word16 @'U.Native @U.Default "Word16"
            , test @Word32 @'U.Native @U.Default "Word32"
            ]
        , testGroup
            "Native pinned"
            [ test @Word8 @'U.Native @U.Pinned "Word8"
            , test @Word16 @'U.Native @U.Pinned "Word16"
            , test @Word32 @'U.Native @U.Pinned "Word32"
            ]
        , testGroup
            "Foreign"
            [ test @Word8 @'U.Foreign @U.Pinned "Word8"
            , test @Word16 @'U.Foreign @U.Pinned "Word16"
            , test @Word32 @'U.Foreign @U.Pinned "Word32"
            ]
        ]
  ]
