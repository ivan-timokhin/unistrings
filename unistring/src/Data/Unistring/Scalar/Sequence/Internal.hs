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

module Data.Unistring.Scalar.Sequence.Internal
  ( Sequence (Sequence)
  , representation
  , uncons
  , fromListN
  , toList
  ) where

import Control.Monad.Trans.State.Strict (execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.List (unfoldr)

import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Unistring.Encoding.Form as EF
import qualified Data.Unistring.Encoding.Form.Internal as EFI
import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Allocator.Unsafe as Allocator
import qualified Data.Unistring.Memory.Array as Array
import Data.Unistring.Memory.Count (CountOf)
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

uncons ::
     Known encoding
  => Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict encoding
  -> Maybe ( ScalarValue
           , Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict encoding)
uncons (Sequence (M.SliceStrict (Slice.NativeSlice (Array.NArray arr) offset len)))
  | len == 0 = Nothing
  | otherwise =
    let !(newOffset, sv) = runIdentity $ EFI.uncheckedDecode arr offset
     in Just
          ( sv
          , Sequence $
            M.SliceStrict $
            Slice.NativeSlice (Array.NArray arr) newOffset $
            len - (newOffset - offset))

toList ::
     Known encoding
  => Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict encoding
  -> [ScalarValue]
{-# INLINE toList #-}
toList = unfoldr uncons

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
