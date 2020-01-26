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

module Behaviour.Unistring.Memory.Allocator
  ( tests
  ) where

import Data.Functor.Classes (Eq1(liftEq))
import Data.Word (Word8)
import GHC.Exts (IsList(fromList))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import qualified Data.Unistring.Memory.Allocator as U
import qualified Data.Unistring.Memory.Array as U
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as U
import qualified Data.Unistring.Memory.Storage as U

tests :: [TestTree]
tests =
  [ testGroup "Adoption" $
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
             in liftEq U.equal adopted (Just array)
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
  ]
