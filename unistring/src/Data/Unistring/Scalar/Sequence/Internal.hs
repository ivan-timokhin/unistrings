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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Unistring.Scalar.Sequence.Internal
  ( Sequence (Sequence)
  , representation
  , fromList
  , fromListN
  , toList
  , Step(Done, Yield, Skip)
  , withStream
  , consChunk
  , empty
  ) where

import Control.Monad.Trans.State.Strict (execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Exts (build)
import GHC.Types (SPEC(SPEC))

import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Unistring.Encoding.Form as EF
import qualified Data.Unistring.Encoding.Form.Internal as EFI
import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Allocator.Unsafe as Allocator
import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import Data.Unistring.Memory.Count (CountOf(CountOf))
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Sequence.Internal as M
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Strictness as Strictness
import Data.Unistring.Scalar.Value (ScalarValue)
import Data.Unistring.Singletons (Known)

newtype Sequence storage allocator ownership strictness encoding =
  Sequence
    (M.Sequence storage allocator ownership strictness (EF.CodeUnit encoding))

representation ::
     Sequence storage allocator ownership strictness encoding
  -> M.Sequence storage allocator ownership strictness (EF.CodeUnit encoding)
representation (Sequence s) = s

consChunk ::
     (Known storage, Known ownership, Known encoding)
  => Sequence storage allocator ownership 'Strictness.Strict encoding
  -> Sequence storage allocator ownership 'Strictness.Lazy encoding
  -> Sequence storage allocator ownership 'Strictness.Lazy encoding
{-# INLINE consChunk #-}
consChunk (Sequence chunk) (Sequence l) = Sequence $ M.consChunk chunk l

empty ::
     (Known storage, Known ownership)
  => Sequence storage allocator ownership 'Strictness.Lazy encoding
{-# INLINE empty #-}
empty = Sequence M.nilL

data Step s a
  = Done
  | Skip s
  | Yield a s

streamToList :: (s -> Step s a) -> s -> [a]
{-# INLINE streamToList #-}
streamToList step s0 =
  build
    (\cons nil ->
       let go !sPEC s =
             case step s of
               Done -> nil
               Skip s' -> go sPEC s'
               Yield a s' -> cons a (go sPEC s')
        in go SPEC s0)

type Streamer s = s -> Step s ScalarValue

withStream ::
     (Known storage, Known ownership, Known strictness, Known encoding)
  => Sequence storage allocator ownership strictness encoding
  -> (forall s. Streamer s -> s -> r)
  -> r
{-# INLINE withStream #-}
withStream (Sequence s) f =
  case M.strictness s of
    Strictness.SStrict ->
      case M.ownership s of
        Ownership.SSlice ->
          let (M.SliceStrict slice) = s
           in f streamSlice slice
        Ownership.SFull ->
          let (M.FullStrict array) = s
           in f streamSlice (Slice.fromArray array)
    Strictness.SLazy -> f streamLazy (Left s)

streamSlice ::
     (Known encoding, Known storage)
  => Streamer (Slice.Slice storage allocator (EF.CodeUnit encoding))
{-# INLINE streamSlice #-}
streamSlice slice
  | len == 0 = Done
  | otherwise =
    let !(sv, slice') = streamSliceNE slice
     in Yield sv slice'
  where
    len = Slice.size slice

streamSliceNE ::
     (Known encoding, Known storage)
  => Slice.Slice storage allocator (EF.CodeUnit encoding)
  -> (ScalarValue, Slice.Slice storage allocator (EF.CodeUnit encoding))
{-# INLINE streamSliceNE #-}
streamSliceNE slice =
  case Slice.storage slice of
    Storage.SNative ->
      let !(Slice.NativeSlice (Array.NArray arr) offset len) = slice
          !(shift, sv) = runIdentity $ EFI.uncheckedDecode arr offset
       in (sv, Slice.sliceUnchecked shift (len - shift) slice)
    Storage.SForeign ->
      let !(Slice.ForeignSlice (Array.FArray (Array.ForeignArray fptr len))) =
            slice
       in unsafeDupablePerformIO $
          Allocator.withForeignPtr fptr $ \ptr -> do
            (shift, sv) <- EFI.uncheckedDecode ptr 0
            pure (sv, Slice.sliceUnchecked shift (len - shift) slice)

streamLazy ::
     (Known e, Known s, Known o)
  => Streamer (Either
               (M.Sequence s a o 'Strictness.Lazy (EF.CodeUnit e))
               ( Slice.Slice s a (EF.CodeUnit e)
               , M.Sequence s a o 'Strictness.Lazy (EF.CodeUnit e)))
{-# INLINE streamLazy #-}
streamLazy (Left chunks) =
  case M.unconsChunk chunks of
    Nothing -> Done
    Just (chunk, rest) ->
      case M.ownership chunk of
        Ownership.SSlice
          | M.SliceStrict slice <- chunk -> Skip $ Right (slice, rest)
        Ownership.SFull
          | M.FullStrict array <- chunk ->
            Skip $ Right (Slice.fromArray array, rest)
streamLazy (Right (slice, rest))
  | Slice.size slice == 0 = Skip $ Left rest
  | otherwise =
    let !(sv, slice') = streamSliceNE slice
     in Yield sv $ Right (slice', rest)

toList ::
     (Known storage, Known ownership, Known strictness, Known encoding)
  => Sequence storage allocator ownership strictness encoding
  -> [ScalarValue]
{-# INLINE toList #-}
toList s = withStream s streamToList

fromListN ::
     (Known encoding, Known ownership, Allocator.Allocator storage allocator)
  => CountOf ScalarValue
  -> [ScalarValue]
  -> Sequence storage allocator ownership 'Strictness.Strict encoding
{-# INLINEABLE fromListN #-}
fromListN n svs =
  Sequence $
  M.withAllocator $ do
    marr <- Allocator.new $ EFI.codeUnitUpperBound n
    let writeSV sv = do
          offset <- get
          newOffset <-
            lift $
            EFI.genericEncode sv $ \diff write -> do
              write marr offset
              pure $ offset + diff
          put newOffset
    finalOffset <- flip execStateT 0 $ traverse_ writeSV svs
    pure (finalOffset, marr)

fromList ::
     (Known encoding, Known ownership, Allocator.Allocator storage allocator)
  => [ScalarValue]
  -> Sequence storage allocator ownership 'Strictness.Strict encoding
{-# INLINEABLE fromList #-}
fromList svs = fromListN (CountOf $ length svs) svs
