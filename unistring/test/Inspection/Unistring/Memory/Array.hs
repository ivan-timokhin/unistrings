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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Inspection.Unistring.Memory.Array
  ( tests
  ) where

import Control.Monad.ST (ST)
import Data.Type.Coercion (Coercion)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word8)
import GHC.Exts (Addr#, Int(I#), Int#, fromListN, toList)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents)
import Test.Inspection (hasNoType)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)

import qualified Data.Unistring.Memory.Allocator as U
import qualified Data.Unistring.Memory.Array as U
import qualified Data.Unistring.Memory.Array.Unsafe as U
import qualified Data.Unistring.Memory.Count as U
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as U
import qualified Data.Unistring.Memory.Storage as U
import qualified Data.Unistring.Singletons as S

import Inspection.TH (hasNoneOfTypes, inspectTest, inspectTests)

tests :: [TestTree]
tests =
  [ testGroup
      "Native array length"
      [ $(inspectTest "Sing" $ 'nativeArrayLength `hasNoType` ''U.Sing)
      , $(inspectTest "Known" $ 'nativeArrayLength `hasNoType` ''S.Known)
      ]
  , testGroup
      "Foreign array length"
      [ $(inspectTest "Sing" $ 'foreignArrayLength `hasNoType` ''U.Sing)
      , $(inspectTest "Known" $ 'foreignArrayLength `hasNoType` ''S.Known)
      ]
  , testGroup
      "Native array toList"
      [ $(inspectTest "Sing" $ 'toListArrayNative `hasNoType` ''U.Sing)
      , $(inspectTest "Known" $ 'toListArrayNative `hasNoType` ''S.Known)
      ]
  , testGroup
      "Foreign array toList"
      [ $(inspectTest "Sing" $ 'toListArrayForeign `hasNoType` ''U.Sing)
      , $(inspectTest "Known" $ 'toListArrayForeign `hasNoType` ''S.Known)
      ]
  , $(inspectTest "Native array toList fuses" $
      'toListArrayNativeFoldr `hasNoType` ''[])
  , expectFail
      $(inspectTest "fromListN fuses" $ 'fromListNEnum `hasNoType` ''[])
  , testGroup
      "Default allocator optimised out"
      [ $(inspectTest "AllocatorM" $ 'fromListNEnum `hasNoType` ''U.AllocatorM)
      , $(inspectTest "Allocator" $ 'fromListNEnum `hasNoType` ''U.Allocator)
      , $(inspectTest "IO" $ 'fromListNEnum `hasNoType` ''IO)
      , $(inspectTest "ST" $ 'fromListNEnum `hasNoType` ''ST)
      ]
  , testGroup
      "Pinned allocator optimised out"
      [ $(inspectTest "AllocatorM" $ 'fromListNEnumP `hasNoType` ''U.AllocatorM)
      , $(inspectTest "Allocator" $ 'fromListNEnumP `hasNoType` ''U.Allocator)
      , $(inspectTest "IO" $ 'fromListNEnumP `hasNoType` ''IO)
      , $(inspectTest "ST" $ 'fromListNEnumP `hasNoType` ''ST)
      ]
  , testGroup
      "Pinned foreign allocator optimised out"
      [ $(inspectTest "AllocatorM" $ 'fromListNEnumF `hasNoType` ''U.AllocatorM)
      , $(inspectTest "Allocator" $ 'fromListNEnumF `hasNoType` ''U.Allocator)
      , $(inspectTest "IO" $ 'fromListNEnumF `hasNoType` ''IO)
      , $(inspectTest "ST" $ 'fromListNEnumF `hasNoType` ''ST)
      ]
  , testGroup
      "Unpack"
      [ testGroup
          "Foreign"
          [ $(inspectTest "ForeignPtr" $
              'mkForeignArray `hasNoType` ''ForeignPtr)
          , $(inspectTest "CountOf" $ 'mkForeignArray `hasNoType` ''U.CountOf)
          , $(inspectTest "Int" $ 'mkForeignArray `hasNoType` ''Int)
          ]
      ]
  , testGroup
      "Free forgetfulness"
      [ $(inspectTest "Coercion" $ 'forgetNativeAllocator `hasNoType` ''Coercion)
      , $(inspectTest "Sing" $ 'forgetNativeAllocator `hasNoType` ''U.Sing)
      , $(inspectTest "Known" $ 'forgetNativeAllocator `hasNoType` ''S.Known)
      ]
  , testGroup
      "Equality"
      [ testGroup
          "Native"
          [ $(inspectTest "Sing" $ 'arrayEqNative `hasNoType` ''U.Sing)
          , $(inspectTest "Known" $ 'arrayEqNative `hasNoType` ''S.Known)
          , $(inspectTest "Primitive" $ 'arrayEqNative `hasNoType` ''U.Primitive)
          ]
      , testGroup
          "Foreign"
          [ $(inspectTest "Sing" $ 'arrayEqForeign `hasNoType` ''U.Sing)
          , $(inspectTest "Known" $ 'arrayEqForeign `hasNoType` ''S.Known)
          , $(inspectTest "Primitive" $
              'arrayEqForeign `hasNoType` ''U.Primitive)
          ]
      , testGroup
          "Mixed"
          [ $(inspectTest "Sing" $ 'arrayEqMixed `hasNoType` ''U.Sing)
          , $(inspectTest "Known" $ 'arrayEqMixed `hasNoType` ''S.Known)
          , $(inspectTest "Primitive" $ 'arrayEqMixed `hasNoType` ''U.Primitive)
          ]
      ]
  , testGroup
      "Convert"
      [ testGroup
          "Native/Default -> Foreign/Pinned"
          $(inspectTests $
            'convertNativeToForeign `hasNoneOfTypes`
            [ ''S.Known
            , ''U.Sing
            , ''U.Allocator
            , ''U.AllocatorM
            , ''U.MutableArray
            , ''U.MonadWithPtr
            , ''U.Primitive
            , ''Typeable
            ])
      ]
  ]

nativeArrayLength :: U.Array 'U.Native alloc Word8 -> U.CountOf Word8
nativeArrayLength = U.size

foreignArrayLength :: U.Array 'U.Foreign alloc Word8 -> U.CountOf Word8
foreignArrayLength = U.size

toListArrayNative :: U.Array 'U.Native alloc Word16 -> [Word16]
toListArrayNative = U.toList

toListArrayForeign :: U.Array 'U.Foreign alloc Word16 -> [Word16]
toListArrayForeign = U.toList

toListArrayNativeFoldr ::
     (U.Allocator 'U.Native alloc, U.Primitive a)
  => (a -> r -> r)
  -> r
  -> U.Array 'U.Native alloc a
  -> r
toListArrayNativeFoldr f z = foldr f z . toList

fromListNEnum :: U.Array 'U.Native U.Default Word8
fromListNEnum = fromListN 10 [1 .. 10]

fromListNEnumP :: U.Array 'U.Native U.Pinned Word8
fromListNEnumP = fromListN 10 [1 .. 10]

fromListNEnumF :: U.Array 'U.Foreign U.Pinned Word8
fromListNEnumF = fromListN 10 [1 .. 10]

mkForeignArray ::
     Addr# -> ForeignPtrContents -> Int# -> U.Array 'U.Foreign allocator a
mkForeignArray ptr cts i =
  U.FArray (U.ForeignArray (ForeignPtr ptr cts) (U.CountOf (I# i)))

forgetNativeAllocator ::
     U.Array 'U.Native allocator a -> U.Array 'U.Native U.Unknown a
forgetNativeAllocator = U.forgetArrayAllocator

arrayEqNative ::
     U.Array 'U.Native allocator Word8
  -> U.Array 'U.Native allocator' Word8
  -> Bool
arrayEqNative = U.equal

arrayEqForeign ::
     U.Array 'U.Foreign allocator Word8
  -> U.Array 'U.Foreign allocator' Word8
  -> Bool
arrayEqForeign = U.equal

arrayEqMixed ::
     U.Array 'U.Native allocator Word8
  -> U.Array 'U.Foreign allocator' Word8
  -> Bool
arrayEqMixed = U.equal

convertNativeToForeign ::
     U.Array 'U.Native U.Default Word8 -> U.Array 'U.Foreign U.Pinned Word8
convertNativeToForeign = U.convert
