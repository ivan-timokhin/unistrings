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

module Behaviour.Unistring.Scalar.Sequence
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck ((===), testProperty)

import qualified Data.Unistring.Memory.Allocator as Allocator
import Data.Unistring.Memory.Count (CountOf(CountOf))
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Strictness as Strictness
import qualified Data.Unistring.Scalar.Sequence.Internal as SSequence

import Behaviour.Unistring.Encoding.Form (SomeEncodingForm(SomeEncodingForm))
import Behaviour.Unistring.Scalar.List (getScalarList)

tests :: [TestTree]
tests =
  [ testGroup
      "IsList"
      [ testProperty "toList . fromListN" $ \(SomeEncodingForm ef) sl ->
          let scalars = getScalarList sl
              scalarsN = length scalars
              ssequence =
                SSequence.fromListN (CountOf scalarsN) scalars `asEncoding` ef
           in SSequence.toList ssequence === scalars
      ]
  ]

asEncoding ::
     SSequence.Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict enc
  -> sing enc
  -> SSequence.Sequence 'Storage.Native Allocator.Default 'Ownership.Slice 'Strictness.Strict enc
asEncoding = const
