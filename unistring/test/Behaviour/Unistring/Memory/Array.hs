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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Behaviour.Unistring.Memory.Array
  ( tests
  ) where

import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList, toList))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary(arbitrary, shrink)
  , (.&&.)
  , (===)
  , choose
  , testProperty
  )
import Data.Type.Equality ((:~:)(Refl), testEquality)

import qualified Data.Unistring.Memory.Allocator as U
import qualified Data.Unistring.Memory.Array as U
import qualified Data.Unistring.Memory.Count as U
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as U
import qualified Data.Unistring.Memory.Storage as U
import qualified Data.Unistring.Singletons as U

import Data.Unistring.Compat.Typeable (TypeRep, typeRep)

import Behaviour.Common ((~/~), (~~~))

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
               in U.getCountOf (U.size array) === length xs
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
  , testGroup "Array Eq" $
    let test ::
             forall a storage1 alloc1 storage2 alloc2.
             ( U.Allocator storage1 alloc1
             , U.Allocator storage2 alloc2
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
                let x :: U.Array storage1 alloc1 a
                    x = fromList xs
                    y :: U.Array storage2 alloc2 a
                    y = fromList xs
                 in x ~~~ y .&&. y ~~~ x
            , testProperty "Not equal" $ \(xs :: [a]) ->
                let x :: U.Array storage1 alloc1 a
                    x = fromList (xs ++ [0])
                    y :: U.Array storage2 alloc2 a
                    y = fromList (xs ++ [1])
                 in x ~/~ y .&&. y ~/~ x
            , testProperty "Not equal length" $ \(xs :: [a]) ->
                let x :: U.Array storage1 alloc1 a
                    x = fromList xs
                    y :: U.Array storage2 alloc2 a
                    y = fromList (xs ++ [1])
                 in x ~/~ y .&&. y ~/~ x
            , testProperty "Random" $ \xs ys ->
                let x :: U.Array storage1 alloc1 a
                    x = fromList xs
                    y :: U.Array storage2 alloc2 a
                    y = fromList ys
                 in (x `U.equal` y) === (xs == ys) .&&. (y `U.equal` x) ===
                    (ys == xs)
            ]
     in [ testGroup
            "Native"
            [ test @Word8 @'U.Native @U.Default @'U.Native @U.Default "Word8"
            , test @Word16 @'U.Native @U.Default @'U.Native @U.Default "Word16"
            , test @Word32 @'U.Native @U.Default @'U.Native @U.Default "Word32"
            ]
        , testGroup
            "Native pinned"
            [ test @Word8 @'U.Native @U.Pinned @'U.Native @U.Pinned "Word8"
            , test @Word16 @'U.Native @U.Pinned @'U.Native @U.Pinned "Word16"
            , test @Word32 @'U.Native @U.Pinned @'U.Native @U.Pinned "Word32"
            ]
        , testGroup
            "Foreign"
            [ test @Word8 @'U.Foreign @U.Pinned @'U.Foreign @U.Pinned "Word8"
            , test @Word16 @'U.Foreign @U.Pinned @'U.Foreign @U.Pinned "Word16"
            , test @Word32 @'U.Foreign @U.Pinned @'U.Foreign @U.Pinned "Word32"
            ]
        , testGroup
            "Mixed"
            [ test @Word8 @'U.Foreign @U.Pinned @'U.Native @U.Default "Word8"
            , test @Word16 @'U.Foreign @U.Pinned @'U.Native @U.Default "Word16"
            , test @Word32 @'U.Foreign @U.Pinned @'U.Native @U.Default "Word32"
            ]
        ]
  , testGroup "Convert" $
    let test ::
             forall a. (U.Primitive a, Show a, Arbitrary a)
          => String
          -> TestTree
        test name =
          testGroup
            name
            [ testProperty "equal before and after" $
              \(SomeArrayType _ _ inputP)
               (SomeArrayType _ _ outputP)
               (xs :: [a]) ->
                let input = fromList xs `asProxyTypeOf` inputP
                    output = U.convert input `asProxyTypeOf` outputP
                 in input ~~~ output
            ]
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  ]

data SomeArrayType a where
  SomeArrayType
    :: U.Allocator storage allocator
    => TypeRep allocator
    -> U.Sing storage
    -> Proxy (U.Array storage allocator a)
    -> SomeArrayType a

deriving instance Show (SomeArrayType a)

someArrayType ::
     forall storage allocator a. U.Allocator storage allocator
  => Proxy (U.Array storage allocator a)
  -> SomeArrayType a
someArrayType = SomeArrayType typeRep U.sing

instance Arbitrary (SomeArrayType a) where
  arbitrary = do
    n <- choose (0 :: Int, 2)
    case n of
      0 -> pure $ someArrayType (Proxy :: Proxy (U.Array 'U.Native U.Default a))
      1 -> pure $ someArrayType (Proxy :: Proxy (U.Array 'U.Native U.Pinned a))
      _ -> pure $ someArrayType (Proxy :: Proxy (U.Array 'U.Foreign U.Pinned a))
  shrink (SomeArrayType alloc storage _)
    | Just Refl <- testEquality alloc (typeRep @U.Default) =
        case storage of
          U.SNative -> []
          U.SForeign -> [SomeArrayType alloc U.SNative Proxy]
    | Just Refl <- testEquality alloc (typeRep @U.Pinned) =
        case storage of
          U.SNative -> [SomeArrayType (typeRep @U.Default) storage Proxy]
          U.SForeign -> [SomeArrayType alloc U.SNative Proxy]
    | otherwise = []
