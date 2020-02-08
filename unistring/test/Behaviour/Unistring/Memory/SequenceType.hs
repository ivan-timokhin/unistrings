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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Behaviour.Unistring.Memory.SequenceType
  ( SomeSequenceType(SomeSequenceType)
  , SequenceType
  ) where

import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink))

import Data.Unistring.Memory.Allocator (Allocator)
import Data.Unistring.Memory.Ownership (Ownership, Sing(SFull, SSlice))
import Data.Unistring.Memory.Strictness (Sing(SLazy, SStrict), Strictness)
import Data.Unistring.Singletons (Known)

import Behaviour.Unistring.Memory.ArrayType
  ( ArrayType
  , SomeArrayType(SomeArrayType)
  )

data SequenceType storage allocator (ownership :: Ownership) (strictness :: Strictness) a =
  SequenceType
    (ArrayType storage allocator a)
    (Sing ownership)
    (Sing strictness)
  deriving (Show)

data SomeSequenceType a where
  SomeSequenceType
    :: (Allocator storage allocator, Known ownership, Known strictness)
    => SequenceType storage allocator ownership strictness a
    -> SomeSequenceType a

deriving instance Show (SomeSequenceType a)

instance Arbitrary (SomeSequenceType a) where
  arbitrary = do
    SomeArrayType arrayType <- arbitrary
    SomeOwnership ownership <- arbitrary
    SomeStrictness strictness <- arbitrary
    pure $ SomeSequenceType (SequenceType arrayType ownership strictness)
  shrink (SomeSequenceType (SequenceType arrayType ownership strictness)) =
    [ SomeSequenceType (SequenceType arrayType ownership strictness')
    | SomeStrictness strictness' <- shrink (SomeStrictness strictness)
    ] ++
    [ SomeSequenceType (SequenceType arrayType ownership' strictness)
    | SomeOwnership ownership' <- shrink (SomeOwnership ownership)
    ] ++
    [ SomeSequenceType (SequenceType arrayType' ownership strictness)
    | SomeArrayType arrayType' <- shrink (SomeArrayType arrayType)
    ]

data SomeOwnership where
  SomeOwnership
    :: Known ownership => Sing (ownership :: Ownership) -> SomeOwnership

instance Arbitrary SomeOwnership where
  arbitrary = do
    p <- arbitrary
    pure $
      if p
        then SomeOwnership SFull
        else SomeOwnership SSlice
  shrink (SomeOwnership SSlice) = [SomeOwnership SFull]
  shrink (SomeOwnership SFull) = []

data SomeStrictness where
  SomeStrictness
    :: Known strictness => Sing (strictness :: Strictness) -> SomeStrictness

instance Arbitrary SomeStrictness where
  arbitrary = do
    p <- arbitrary
    pure $
      if p
        then SomeStrictness SStrict
        else SomeStrictness SLazy
  shrink (SomeStrictness SLazy) = [SomeStrictness SStrict]
  shrink (SomeStrictness SStrict) = []
