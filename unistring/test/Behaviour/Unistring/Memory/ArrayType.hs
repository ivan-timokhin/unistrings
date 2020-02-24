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
{-# LANGUAGE KindSignatures #-}

module Behaviour.Unistring.Memory.ArrayType
  ( ArrayType
  , SomeArrayType(SomeArrayType)
  , changeType
  ) where

import Data.Type.Equality ((:~:)(Refl), testEquality)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), choose)

import Data.Unistring.Memory.Allocator (Allocator, Default, Pinned)
import Data.Unistring.Memory.Storage
  ( Sing(SForeign, SNative)
  , Storage(Foreign, Native)
  )
import Data.Unistring.Singletons (Known(sing))

import Data.Unistring.Compat.Typeable (TypeRep, typeRep)

data ArrayType (storage :: Storage) allocator a where
  ArrayType
    :: TypeRep allocator -> Sing storage -> ArrayType storage allocator a
  deriving (Show)

changeType :: ArrayType storage allocator a -> ArrayType storage allocator b
changeType (ArrayType tr st) = ArrayType tr st

data SomeArrayType a where
  SomeArrayType
    :: Allocator storage allocator
    => ArrayType storage allocator a
    -> SomeArrayType a

deriving instance Show (SomeArrayType a)

mkSAT ::
     forall storage allocator a. Allocator storage allocator
  => SomeArrayType a
mkSAT = SomeArrayType (ArrayType (typeRep @allocator) (sing @storage))

instance Arbitrary (SomeArrayType a) where
  arbitrary = do
    n <- choose (0 :: Int, 2)
    case n of
      0 -> pure $ mkSAT @'Native @Default
      1 -> pure $ mkSAT @'Native @Pinned
      _ -> pure $ mkSAT @'Foreign @Pinned
  shrink (SomeArrayType (ArrayType alloc storage))
    | Just Refl <- testEquality alloc (typeRep @Default) =
      case storage of
        SNative -> []
        SForeign -> [mkSAT @'Native @Default]
    | Just Refl <- testEquality alloc (typeRep @Pinned) =
      case storage of
        SNative -> [mkSAT @'Native @Default]
        SForeign -> [mkSAT @'Native @Pinned]
    | otherwise = []
