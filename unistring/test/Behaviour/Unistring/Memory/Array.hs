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

module Behaviour.Unistring.Memory.Array
  ( tests
  ) where

import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList, toList))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, (.&&.), (===), testProperty)

import qualified Data.Unistring.Memory.Array as U
import qualified Data.Unistring.Memory.Count as U
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as U

import Behaviour.Common ((~/~), (~~~))
import Behaviour.Unistring.Memory.ArrayType
  ( ArrayType
  , SomeArrayType(SomeArrayType)
  )

tests :: [TestTree]
tests =
  [ testGroup "IsList" $
    let test ::
             forall a. (U.Primitive a, Eq a, Show a, Arbitrary a)
          => [TestTree]
        test =
          [ testProperty "toList . fromList == id" $ \(SomeArrayType t) (xs :: [a]) ->
              let array = fromList xs `asArrayType` t
               in toList array === xs
          , testProperty "length . fromList == length" $ \(SomeArrayType t) (xs :: [a]) ->
              let array = fromList xs `asArrayType` t
               in U.getCountOf (U.size array) === length xs
          ]
     in [ testGroup "Word8" (test @Word8)
        , testGroup "Word16" (test @Word16)
        , testGroup "Word32" (test @Word32)
        ]
  , testGroup "Array Eq" $
    let test ::
             forall a. (U.Primitive a, Num a, Arbitrary a, Show a, Eq a)
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "Equal" $ \(SomeArrayType lt) (SomeArrayType rt) (xs :: [a]) ->
                let x = fromList xs `asArrayType` lt
                    y = fromList xs `asArrayType` rt
                 in x ~~~ y .&&. y ~~~ x
            , testProperty "Not equal" $ \(SomeArrayType lt) (SomeArrayType rt) (xs :: [a]) ->
                let x = fromList (xs ++ [0]) `asArrayType` lt
                    y = fromList (xs ++ [1]) `asArrayType` rt
                 in x ~/~ y .&&. y ~/~ x
            , testProperty "Not equal length" $ \(SomeArrayType lt) (SomeArrayType rt) (xs :: [a]) ->
                let x = fromList xs `asArrayType` lt
                    y = fromList (xs ++ [1]) `asArrayType` rt
                 in x ~/~ y .&&. y ~/~ x
            , testProperty "Random" $ \(SomeArrayType lt) (xs :: [a]) (SomeArrayType rt) ys ->
                let x = fromList xs `asArrayType` lt
                    y = fromList ys `asArrayType` rt
                 in (x `U.equal` y) === (xs == ys) .&&. (y `U.equal` x) ===
                    (ys == xs)
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  , testGroup "Convert" $
    let test ::
             forall a. (U.Primitive a, Show a, Arbitrary a)
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "equal before and after" $ \(SomeArrayType inputP) (SomeArrayType outputP) (xs :: [a]) ->
                let input = fromList xs `asArrayType` inputP
                    output = U.convert input `asArrayType` outputP
                 in input ~~~ output
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  , testGroup "Empty" $
    let test ::
             forall a. (U.Primitive a, Show a)
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "Equal to fromList []" $ \(SomeArrayType t) ->
                let empty = U.empty `asArrayType` t
                 in empty === fromList ([] :: [a])
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  , testGroup "Append" $
    let test ::
             forall a. (U.Primitive a, Show a, Arbitrary a)
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "isomorphic to lists" $
              \(SomeArrayType xt)
               (SomeArrayType yt)
               (SomeArrayType zt)
               (xs :: [a])
               (ys :: [a]) ->
                let xa = fromList xs `asArrayType` xt
                    ya = fromList ys `asArrayType` yt
                    za = U.append xa ya `asArrayType` zt
                    zs = xs ++ ys
                 in za === fromList zs
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  ]

asArrayType ::
     U.Array storage allocator a
  -> ArrayType storage allocator a
  -> U.Array storage allocator a
asArrayType = const
