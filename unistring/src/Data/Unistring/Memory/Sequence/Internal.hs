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
{-# LANGUAGE RankNTypes #-}

module Data.Unistring.Memory.Sequence.Internal
  ( Sequence(FullStrict, SliceStrict, ConsNF, NilNF, ConsFF, NilFF,
         ConsNS, NilNS, ConsFS, NilFS)
  , toList
  , equal
  , nilL
  , unconsChunk
  , consChunk
  , chunks
  , storage
  , ownership
  , strictness
  , withAllocator
  , withAllocatorT
  ) where

import Data.List (unfoldr)
import Data.Maybe (isNothing)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Traversable (for)
import Data.Functor.Identity (Identity(Identity, runIdentity))

import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Slice.Internal as Slice -- TODO
import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Strictness as Strictness
import qualified Data.Unistring.Memory.Allocator.Unsafe as Allocator
import qualified Data.Unistring.Memory.Allocator as Allocator
import Data.Unistring.Singletons (Known(sing), Sing)
import Data.Unistring.Memory.Primitive.Class.Unsafe (Primitive)
import Data.Unistring.Memory.Count (CountOf)

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

instance (Known storage, Known ownership, Known strictness, Primitive a) =>
         Eq (Sequence storage allocator ownership strictness a) where
  (==) = equal

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
  (Known storage, Known ownership, Primitive a)
  => Sequence storage allocator ownership 'Strictness.Strict a
  -> Sequence storage allocator ownership 'Strictness.Lazy a
  -> Sequence storage allocator ownership 'Strictness.Lazy a
{-# INLINE consChunk #-}
consChunk c cs
  | sizeStrict c == 0 = cs
  | otherwise =
  case (sing @storage, sing @ownership) of
    (Storage.SNative, Ownership.SFull) -> ConsNF (getFullStrict c) cs
    (Storage.SNative, Ownership.SSlice) -> ConsNS (getSliceStrict c) cs
    (Storage.SForeign, Ownership.SFull) -> ConsFF (getFullStrict c) cs
    (Storage.SForeign, Ownership.SSlice) -> ConsFS (getSliceStrict c) cs
  where
    getFullStrict (FullStrict a) = a
    getSliceStrict (SliceStrict a) = a

chunks ::
     (Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Lazy a
  -> [Sequence storage allocator ownership 'Strictness.Strict a]
{-# INLINE chunks #-}
chunks = unfoldr unconsChunk

sizeStrict ::
     (Known storage, Known ownership, Primitive a)
  => Sequence storage allocator ownership 'Strictness.Strict a
  -> CountOf a
{-# INLINEABLE sizeStrict #-}
sizeStrict s =
  case ownership s of
    Ownership.SFull
      | (FullStrict array) <- s -> Array.size array
    Ownership.SSlice
      | (SliceStrict slice) <- s -> Slice.size slice

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

equal ::
     ( Known storage1
     , Known storage2
     , Known ownership1
     , Known ownership2
     , Known strictness1
     , Known strictness2
     , Primitive a)
     => Sequence storage1 allocator1 ownership1 strictness1 a
     -> Sequence storage2 allocator2 ownership2 strictness2 a
     -> Bool
{-# INLINEABLE equal #-}
equal s1 s2 =
  case (strictness s1, strictness s2) of
    (Strictness.SStrict, Strictness.SStrict) -> s1 `equalStrict` s2
    (Strictness.SStrict, Strictness.SLazy) -> s1 `equalMixed` s2
    (Strictness.SLazy, Strictness.SStrict) -> s2 `equalMixed` s1
    (Strictness.SLazy, Strictness.SLazy) -> s1 `equalLazy` s2

equalStrict ::
     ( Known storage1
     , Known storage2
     , Known ownership1
     , Known ownership2
     , Primitive a
     )
  => Sequence storage1 allocator1 ownership1 'Strictness.Strict a
  -> Sequence storage2 allocator2 ownership2 'Strictness.Strict a
  -> Bool
{-# INLINEABLE equalStrict #-}
equalStrict s1 s2 =
  case (ownership s1, ownership s2) of
    (Ownership.SFull, Ownership.SFull)
      | FullStrict arr1 <- s1
      , FullStrict arr2 <- s2 -> Array.equal arr1 arr2
    _ -> asSlice s1 `Slice.equal` asSlice s2

equalMixed ::
     ( Known storage1
     , Known storage2
     , Known ownership1
     , Known ownership2
     , Primitive a
     )
  => Sequence storage1 allocator1 ownership1 'Strictness.Strict a
  -> Sequence storage2 allocator2 ownership2 'Strictness.Lazy a
  -> Bool
{-# INLINEABLE equalMixed #-}
equalMixed = go . asSlice
  where
    go slice sq =
      case unconsChunk sq of
        Nothing -> Slice.size slice == 0
        Just (strict, rest) ->
          let headSlice = asSlice strict
              headSize = Slice.size headSlice
              (prefix, suffix) = Slice.splitAt headSize slice
           in prefix `Slice.equal` headSlice && go suffix rest

equalLazy ::
     ( Known storage1
     , Known storage2
     , Known ownership1
     , Known ownership2
     , Primitive a
     )
  => Sequence storage1 allocator1 ownership1 'Strictness.Lazy a
  -> Sequence storage2 allocator2 ownership2 'Strictness.Lazy a
  -> Bool
{-# INLINEABLE equalLazy #-}
equalLazy = go0
  where
    go0 sq1 sq2 =
      case unconsChunk sq1 of
        Nothing -> isNothing $ unconsChunk sq2
        Just (h1, t1) -> go1 (asSlice h1) t1 sq2
    go1 h1 t1 sq2 =
      case unconsChunk sq2 of
        Nothing -> False
        Just (h2, t2) -> go2 h1 t1 (asSlice h2) t2
    go1' sq1 h2 t2 =
      case unconsChunk sq1 of
        Nothing -> False
        Just (h1, t1) -> go2 (asSlice h1) t1 h2 t2
    go2 h1 t1 h2 t2 =
      case size1 `compare` size2 of
        EQ -> h1 `Slice.equal` h2 && go0 t1 t2
        LT
          | (h2prefix, h2suffix) <- Slice.splitAt size1 h2 ->
            h1 `Slice.equal` h2prefix && go1' t1 h2suffix t2
        GT
          | (h1prefix, h1suffix) <- Slice.splitAt size2 h1 ->
            h1prefix `Slice.equal` h2 && go1 h1suffix t1 t2
      where
        size1 = Slice.size h1
        size2 = Slice.size h2

asSlice ::
     (Primitive a, Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Strict a
  -> Slice.Slice storage allocator a
{-# INLINE asSlice #-}
asSlice s1 =
  case ownership s1 of
    Ownership.SFull
      | FullStrict arr <- s1 -> Slice.fromArray arr
    Ownership.SSlice
      | SliceStrict sl <- s1 -> sl

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

withAllocator ::
     forall storage allocator ownership a.
     (Allocator.Allocator storage allocator, Known ownership, Primitive a)
  => (forall m arr. Allocator.AllocatorM arr m =>
                      m (CountOf a, arr a))
  -> Sequence storage allocator ownership 'Strictness.Strict a
{-# INLINE withAllocator #-}
withAllocator body = runIdentity $ withAllocatorT (Identity <$> body)

withAllocatorT ::
     forall storage allocator ownership a t.
     ( Allocator.Allocator storage allocator
     , Known ownership
     , Primitive a
     , Traversable t
     )
  => (forall m arr. Allocator.AllocatorM arr m =>
                      m (t (CountOf a, arr a)))
  -> t (Sequence storage allocator ownership 'Strictness.Strict a)
{-# INLINE withAllocatorT #-}
withAllocatorT body =
  case sing @ownership of
    Ownership.SFull ->
      fmap FullStrict $
      Allocator.withAllocatorT $ do
        arrays <- body
        traverse (\(n, array) -> Allocator.shrink array n) arrays
    Ownership.SSlice ->
      fmap
        (\(n, array) ->
           SliceStrict $ Slice.sliceUnchecked 0 n $ Slice.fromArray array) $
      getCompose $
      Allocator.withAllocatorT $ do
        arrays <- body
        shrunk <-
          for arrays $ \(n, array) ->
            Allocator.tryShrink array n >>= \case
              Just shrunk -> pure (n, shrunk)
              Nothing -> pure (n, array)
        pure $ Compose shrunk
