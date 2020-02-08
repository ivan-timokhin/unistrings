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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Behaviour.Unistring.Memory.Sequence
  ( tests
  ) where

import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary
  , (===)
  , testProperty
  , NonNegative(NonNegative)
#if MIN_VERSION_tasty_quickcheck(0, 10, 1)
  , InfiniteList(getInfiniteList)
#endif
  )

import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as Primitive
import qualified Data.Unistring.Memory.Sequence.Internal as Sequence
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Strictness as Strictness
import Data.Unistring.Singletons (Known(sing))

import Behaviour.Unistring.Memory.SequenceType
  ( SequenceType
  , SomeSequenceType(SomeSequenceType)
  )
import Behaviour.Common ((~~~))

tests :: [TestTree]
tests =
  [ testGroup "toList" $
    let test ::
             forall a. (Primitive.Primitive a, Eq a, Show a, Arbitrary a)
          => [TestTree]
        test = [
          testProperty "toList . fromLists" $
          \(SomeSequenceType t)
           prefixes
           (xss :: [[a]])
           suffixes ->
            let s = fromLists
                    (infiniteList prefixes)
                    xss
                    (infiniteList suffixes)
                  `asSequenceType` t
             in Sequence.toList s === concat xss
          ]
     in [ testGroup "Word8" (test @Word8)
        , testGroup "Word16" (test @Word16)
        , testGroup "Word32" (test @Word32)
        ]
  , testGroup "Eq" $
    let test ::
             forall a. (Primitive.Primitive a, Eq a, Show a, Arbitrary a)
          => [TestTree]
        test = [
          testProperty "Equal" $
          \(SomeSequenceType t1)
           (SomeSequenceType t2)
           prefixes1
           prefixes2
           (xs :: [a])
           lengths1
           lengths2
           suffixes1
           suffixes2 ->
            let s1 =
                  fromLists
                  (infiniteList prefixes1)
                  (intoChunks lengths1 xs)
                  (infiniteList suffixes1)
                  `asSequenceType` t1
                s2 =
                  fromLists
                  (infiniteList prefixes2)
                  (intoChunks lengths2 xs)
                  (infiniteList suffixes2)
                  `asSequenceType` t2
             in s1 ~~~ s2
         , testProperty "Random" $
           \(SomeSequenceType t1)
            (SomeSequenceType t2)
            prefixes1
            prefixes2
            (xss :: [[a]])
            (yss :: [[a]])
            suffixes1
            suffixes2 ->
             let s1 =
                   fromLists
                   (infiniteList prefixes1)
                   xss
                   (infiniteList suffixes1)
                   `asSequenceType` t1
                 s2 =
                   fromLists
                   (infiniteList prefixes2)
                   yss
                   (infiniteList suffixes2)
                   `asSequenceType` t2
              in (s1 `Sequence.equal` s2) === (concat xss == concat yss)
         ]
    in [ testGroup "Word8" $ test @Word8
       , testGroup "Word16" $ test @Word16
       , testGroup "Word32" $ test @Word32
       ]
  ]

#if MIN_VERSION_tasty_quickcheck(0, 10, 1)
infiniteList :: InfiniteList a -> [a]
infiniteList = getInfiniteList
#else
infiniteList :: Monoid a => [a] -> [a]
infiniteList [] = repeat mempty
infiniteList xs = cycle xs
#endif

intoChunks :: [NonNegative Int] -> [a] -> [[a]]
intoChunks [] xs = [xs]
intoChunks (NonNegative n:ns) xs = prefix : intoChunks ns suffix
  where
    (prefix, suffix) = splitAt n xs

fromLists ::
     forall storage allocator ownership strictness a.
     ( Allocator.Allocator storage allocator
     , Known ownership
     , Known strictness
     , Primitive.Primitive a
     )
  => [[a]] -- Should be infinite
  -> [[a]]
  -> [[a]] -- Should be infinite
  -> Sequence.Sequence storage allocator ownership strictness a
fromLists prefixes xss suffixes =
  case sing @strictness of
    Strictness.SStrict ->
      case sing @ownership of
        Ownership.SSlice ->
          let prefix = head prefixes
              xs = concat xss
              suffix = head suffixes
           in from3Lists prefix xs suffix
        Ownership.SFull -> Sequence.FullStrict $ fromList $ concat xss
    Strictness.SLazy ->
      foldr @[] Sequence.consChunk Sequence.nilL $
      case sing @ownership of
        Ownership.SSlice -> zipWith3 from3Lists prefixes xss suffixes
        Ownership.SFull -> map (Sequence.FullStrict . fromList) xss

from3Lists ::
     (Primitive.Primitive a, Allocator.Allocator storage allocator)
  => [a]
  -> [a]
  -> [a]
  -> Sequence.Sequence storage allocator 'Ownership.Slice 'Strictness.Strict a
from3Lists prefix xs suffix =
  Sequence.SliceStrict $
  Slice.sliceUnchecked
    (Count.CountOf $ length prefix)
    (Count.CountOf $ length xs) $
  fromList (prefix ++ xs ++ suffix)

asSequenceType ::
     Sequence.Sequence storage allocator ownership strictness a
  -> SequenceType storage allocator ownership strictness a
  -> Sequence.Sequence storage allocator ownership strictness a
asSequenceType = const
