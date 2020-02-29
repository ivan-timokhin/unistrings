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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Behaviour.Unistring.Scalar.Sequence
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup, adjustOption)
import Test.Tasty.QuickCheck
  ( QuickCheckMaxSize(QuickCheckMaxSize)
  , (===)
  , testProperty
  )
import GHC.Exts (fromList, toList)

import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Strictness as Strictness
import qualified Data.Unistring.Scalar.Sequence.Internal as SSequence
import Data.Unistring.Singletons (Known(sing))

import Behaviour.Unistring.Scalar.List (ScalarList, getScalarList)
import Behaviour.Unistring.Scalar.SequenceType
  ( SequenceType
  , SomeSequenceType(SomeSequenceType)
  )
import Behaviour.Common ((~/~), (~~~))

tests :: [TestTree]
tests =
  [ testGroup
      "IsList"
      [ testProperty "toList . fromLists" $ \(SomeSequenceType st) sls ->
          let s = fromLists sls `asSequenceType` st
           in SSequence.toList s === concatMap getScalarList sls
      , testProperty "toList . fromList (short)" $ \(SomeSequenceType st) sl ->
          let s = fromList (getScalarList sl) `asSequenceType` st
           in toList s === getScalarList sl
      -- We need to test on lists long enough that fromList allocates
      -- several chunks.
      , adjustOption
          (\(QuickCheckMaxSize sz) -> QuickCheckMaxSize $ max 10000 sz) $
        testProperty "toList . fromList (long)" $ \(SomeSequenceType st) sl ->
          let s = fromList (getScalarList sl) `asSequenceType` st
           in toList s === getScalarList sl
      ]
  , testGroup "Equality" $
    let test :: [TestTree]
        test =
          [ testProperty "Equal" $ \(SomeSequenceType st) sl ->
              let s1 = fromList (getScalarList sl) `asSequenceType` st
                  s2 = fromList (getScalarList sl) `asSequenceType` st
               in s1 ~~~ s2
          , testProperty "Random" $ \(SomeSequenceType st) sl1 sl2 ->
              let s1 = fromList (getScalarList sl1) `asSequenceType` st
                  s2 = fromList (getScalarList sl2) `asSequenceType` st
               in if sl1 == sl2
                    then s1 ~~~ s2
                    else s1 ~/~ s2
          ]
     in [ testGroup "Small" test
        , adjustOption
            (\(QuickCheckMaxSize sz) -> QuickCheckMaxSize $ max 10000 sz) $
          testGroup "Large" test
        ]
  ]

fromLists ::
     forall storage allocator ownership strictness encoding.
     ( Allocator.Allocator storage allocator
     , Known ownership
     , Known strictness
     , Known encoding
     )
  => [ScalarList]
  -> SSequence.Sequence storage allocator ownership strictness encoding
fromLists svss =
  case sing @strictness of
    Strictness.SStrict -> SSequence.fromList $ concatMap getScalarList svss
    Strictness.SLazy ->
      foldr
        (SSequence.consChunk . SSequence.fromList . getScalarList)
        SSequence.empty
        svss

asSequenceType ::
     SSequence.Sequence storage allocator ownership strictness encoding
  -> SequenceType storage allocator ownership strictness encoding
  -> SSequence.Sequence storage allocator ownership strictness encoding
asSequenceType = const
