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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Data.Unistring.Memory.Sequence.Internal
  ( Sequence(FullStrict, SliceStrict, ConsNF, NilNF, ConsFF, NilFF,
         ConsNS, NilNS, ConsFS, NilFS)
  , toList
  , nilL
  , unconsChunk
  , consChunk
  , chunks
  , storage
  , ownership
  , strictness
  ) where

import Data.List (unfoldr)

import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Slice.Internal as Slice -- TODO
import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Strictness as Strictness
import Data.Unistring.Singletons (Known(sing), Sing)
import Data.Unistring.Memory.Primitive.Class.Unsafe (Primitive)

data family
     Sequence
       (storage :: Storage.Storage)
       allocator
       (ownership :: Ownership.Ownership)
       (strictness :: Strictness.Strictness)
       a

newtype instance
        Sequence
          storage
          allocator
          'Ownership.Full
          'Strictness.Strict
          a
        = FullStrict (Array.Array storage allocator a)

newtype instance
        Sequence
          storage
          allocator
          'Ownership.Slice
          'Strictness.Strict
          a
        = SliceStrict (Slice.Slice storage allocator a)

data instance
     Sequence
       'Storage.Native
       allocator
       'Ownership.Full
       'Strictness.Lazy
       a
     = NilNF
     | ConsNF {-# UNPACK #-}!(Array.Array 'Storage.Native allocator a)
              (Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Lazy a)

data instance
     Sequence
       'Storage.Foreign
       allocator
       'Ownership.Full
       'Strictness.Lazy
       a
     = NilFF
     | ConsFF {-# UNPACK #-}!(Array.Array 'Storage.Foreign allocator a)
              (Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Lazy a)

data instance
     Sequence
       'Storage.Native
       allocator
       'Ownership.Slice
       'Strictness.Lazy
       a
     = NilNS
     | ConsNS {-# UNPACK #-}!(Slice.Slice 'Storage.Native allocator a)
              (Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Lazy a)

data instance
     Sequence
       'Storage.Foreign
       allocator
       'Ownership.Slice
       'Strictness.Lazy
       a
     = NilFS
     | ConsFS {-# UNPACK #-}!(Slice.Slice 'Storage.Foreign allocator a)
              (Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Lazy a)

instance (Known storage, Known ownership, Known strictness, Primitive a, Show a) =>
         Show (Sequence storage allocator ownership strictness a) where
  showsPrec p = showsPrec p . toList

nilL :: forall storage allocator ownership a.
     (Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Lazy a
{-# INLINE nilL #-}
nilL = case (sing @storage, sing @ownership) of
  (Storage.SNative, Ownership.SFull) -> NilNF
  (Storage.SNative, Ownership.SSlice) -> NilNS
  (Storage.SForeign, Ownership.SFull) -> NilFF
  (Storage.SForeign, Ownership.SSlice) -> NilFS

unconsChunk ::
  forall storage allocator ownership a.
     (Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Lazy a
  -> Maybe ( Sequence storage allocator ownership 'Strictness.Strict a
           , Sequence storage allocator ownership 'Strictness.Lazy a)
{-# INLINE unconsChunk #-}
unconsChunk =
  case (sing @storage, sing @ownership) of
    (Storage.SNative, Ownership.SFull) ->
      \case
        ConsNF x xs -> Just (FullStrict x, xs)
        NilNF -> Nothing
    (Storage.SNative, Ownership.SSlice) ->
      \case
        ConsNS x xs -> Just (SliceStrict x, xs)
        NilNS -> Nothing
    (Storage.SForeign, Ownership.SFull) ->
      \case
        ConsFF x xs -> Just (FullStrict x, xs)
        NilFF -> Nothing
    (Storage.SForeign, Ownership.SSlice) ->
      \case
        ConsFS x xs -> Just (SliceStrict x, xs)
        NilFS -> Nothing

consChunk :: forall storage allocator ownership a.
  (Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Strict a
  -> Sequence storage allocator ownership 'Strictness.Lazy a
  -> Sequence storage allocator ownership 'Strictness.Lazy a
{-# INLINE consChunk #-}
consChunk = case (sing @storage, sing @ownership) of
  (Storage.SNative, Ownership.SFull) -> ConsNF . getFullStrict
  (Storage.SNative, Ownership.SSlice) -> ConsNS . getSliceStrict
  (Storage.SForeign, Ownership.SFull) -> ConsFF . getFullStrict
  (Storage.SForeign, Ownership.SSlice) -> ConsFS . getSliceStrict
  where
    getFullStrict (FullStrict a) = a
    getSliceStrict (SliceStrict a) = a

chunks ::
     (Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Lazy a
  -> [Sequence storage allocator ownership 'Strictness.Strict a]
{-# INLINE chunks #-}
chunks = unfoldr unconsChunk

toList ::
     (Known storage, Known ownership, Known strictness, Primitive a)
  => Sequence storage allocator ownership strictness a
  -> [a]
{-# INLINE toList #-}
toList xs =
  case strictness xs of
    Strictness.SStrict -> toListStrict xs
    Strictness.SLazy -> toListLazy xs

toListStrict ::
     (Known storage, Known ownership, Primitive a)
  => Sequence storage allocator ownership 'Strictness.Strict a
  -> [a]
{-# INLINE toListStrict #-}
toListStrict xs =
  case ownership xs of
    Ownership.SFull
      | FullStrict arr <- xs -> Array.toList arr
    Ownership.SSlice
      | SliceStrict slice <- xs -> Slice.toList slice

toListLazy ::
     (Known storage, Known ownership, Primitive a)
  => Sequence storage allocator ownership 'Strictness.Lazy a
  -> [a]
{-# INLINE toListLazy #-}
toListLazy = concatMap toListStrict . chunks

storage ::
     Known storage
  => Sequence storage allocator ownership strictness a
  -> Sing storage
{-# INLINE storage #-}
storage = const sing

ownership ::
     Known ownership
  => Sequence storage allocator ownership strictness a
  -> Sing ownership
{-# INLINE ownership #-}
ownership = const sing

strictness ::
     Known strictness
  => Sequence storage allocator ownership strictness a
  -> Sing strictness
{-# INLINE strictness #-}
strictness = const sing
