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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Data.Unistring.Scalar.Sequence.Internal
  ( Sequence (Sequence)
  , representation
  , storage
  , ownership
  , strictness
  , encodingForm
  , equal
  , fromList
  , fromListNStrict
  , toList
  , Step(Done, Yield, Skip)
  , Stream(Stream)
  , stream
  , consChunk
  , empty
  ) where

import Control.Monad.Trans.State.Strict (execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import qualified GHC.Exts as E
import GHC.Types (SPEC(SPEC))
import Data.Type.Equality ((:~:)(Refl), testEquality)

import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Unistring.Encoding.Form as EF
import qualified Data.Unistring.Encoding.Form.Internal as EFI
import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Allocator.Unsafe as Allocator
import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import Data.Unistring.Memory.Count (CountOf(CountOf), ByteCount)
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Sequence.Internal as M
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Strictness as Strictness
import Data.Unistring.Scalar.Value (ScalarValue)
import Data.Unistring.Singletons (Known(sing), Sing)
import Data.Unistring.Memory.Primitive.Class.Unsafe (inElements)
import Data.Unistring.Internal.IO (readOnlyPerformIO)

newtype Sequence storage allocator ownership strictness encoding =
  Sequence
    (M.Sequence storage allocator ownership strictness (EF.CodeUnit encoding))

instance (Known storage, Known ownership, Known strictness, Known encoding) =>
         Show (Sequence storage allocator ownership strictness encoding) where
  showsPrec p = showsPrec p . toList

representation ::
     Sequence storage allocator ownership strictness encoding
  -> M.Sequence storage allocator ownership strictness (EF.CodeUnit encoding)
representation (Sequence s) = s

storage ::
     Known storage
  => Sequence storage allocator ownership strictness encoding
  -> Sing storage
{-# INLINE storage #-}
storage = const sing

ownership ::
     Known ownership
  => Sequence storage allocator ownership strictness encoding
  -> Sing ownership
{-# INLINE ownership #-}
ownership = const sing

strictness ::
     Known strictness
  => Sequence storage allocator ownership strictness encoding
  -> Sing strictness
{-# INLINE strictness #-}
strictness = const sing

encodingForm ::
     Known encoding
  => Sequence storage allocator ownership strictness encoding
  -> Sing encoding
{-# INLINE encodingForm #-}
encodingForm = const sing

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
  | Skip !s
  | Yield !a !s

data Stream a where
  Stream :: (s -> Step s a) -> s -> Stream a

streamToList :: Stream a -> [a]
{-# INLINE streamToList #-}
streamToList (Stream step s0) =
  E.build
    (\cons nil ->
       let go !sPEC s =
             case step s of
               Done -> nil
               Skip s' -> go sPEC s'
               Yield a s' -> cons a (go sPEC s')
        in go SPEC s0)

listToStream :: [a] -> Stream a
{-# INLINE listToStream #-}
{- HLINT ignore listToStream -}
listToStream xs = Stream step xs
  where
    step (y:ys) = Yield y ys
    step [] = Done

type Streamer s = s -> Step s ScalarValue

stream
  :: (Known storage, Known ownership, Known strictness, Known encoding)
  => Sequence storage allocator ownership strictness encoding
  -> Stream ScalarValue
{-# INLINE [0] stream #-}
stream s = case strictness s of
  Strictness.SStrict -> case ownership s of
    Ownership.SSlice -> streamSlice s
    Ownership.SFull  -> streamFull s
  Strictness.SLazy -> streamLazy s

{-# RULES
"stream -> streamSlice" stream = streamSlice
"stream -> streamFull" stream = streamFull
"stream -> streamLazy" stream = streamLazy
  #-}

streamSlice
  :: (Known encoding, Known storage)
  => Sequence
       storage
       allocator
       'Ownership.Slice
       'Strictness.Strict
       encoding
  -> Stream ScalarValue
{-# INLINE streamSlice #-}
streamSlice (Sequence (M.SliceStrict slice)) = Stream streamerSlice slice

streamFull
  :: (Known encoding, Known storage)
  => Sequence storage allocator 'Ownership.Full 'Strictness.Strict encoding
  -> Stream ScalarValue
{-# INLINE streamFull #-}
streamFull (Sequence (M.FullStrict array)) =
  Stream streamerSlice (Slice.fromArray array)

streamLazy :: (Known e, Known s, Known o) =>
  Sequence s a o 'Strictness.Lazy e -> Stream ScalarValue
{-# INLINE streamLazy #-}
streamLazy (Sequence s) = Stream streamerLazy (Left s)

streamerSlice ::
     (Known encoding, Known storage)
  => Streamer (Slice.Slice storage allocator (EF.CodeUnit encoding))
{-# INLINE streamerSlice #-}
streamerSlice slice
  | len == 0 = Done
  | otherwise =
    let !(sv, slice') = streamerSliceNE slice
     in Yield sv slice'
  where
    len = Slice.size slice

streamerSliceNE ::
     (Known encoding, Known storage)
  => Slice.Slice storage allocator (EF.CodeUnit encoding)
  -> (ScalarValue, Slice.Slice storage allocator (EF.CodeUnit encoding))
{-# INLINE streamerSliceNE #-}
streamerSliceNE slice =
  case Slice.storage slice of
    Storage.SNative ->
      let !(Slice.NativeSlice (Array.NArray arr) offset len) = slice
          !(shift, sv) = runIdentity $ EFI.uncheckedDecode arr offset
       in (sv, Slice.sliceUnchecked shift (len - shift) slice)
    Storage.SForeign ->
      let !(Slice.ForeignSlice (Array.FArray (Array.ForeignArray fptr len))) =
            slice
       in readOnlyPerformIO $
          Allocator.withForeignPtr fptr $ \ptr -> do
            (shift, sv) <- EFI.uncheckedDecode ptr 0
            pure (sv, Slice.sliceUnchecked shift (len - shift) slice)

streamerLazy ::
     (Known e, Known s, Known o)
  => Streamer (Either
               (M.Sequence s a o 'Strictness.Lazy (EF.CodeUnit e))
               ( Slice.Slice s a (EF.CodeUnit e)
               , M.Sequence s a o 'Strictness.Lazy (EF.CodeUnit e)))
{-# INLINE streamerLazy #-}
streamerLazy (Left chunks) =
  case M.unconsChunk chunks of
    Nothing -> Done
    Just (chunk, rest) ->
      case M.ownership chunk of
        Ownership.SSlice
          | M.SliceStrict slice <- chunk -> Skip $ Right (slice, rest)
        Ownership.SFull
          | M.FullStrict array <- chunk ->
            Skip $ Right (Slice.fromArray array, rest)
streamerLazy (Right (slice, rest))
  | Slice.size slice == 0 = Skip $ Left rest
  | otherwise =
    let (!sv, !slice') = streamerSliceNE slice
     in Yield sv $ Right (slice', rest)

unstreamLazy ::
     forall storage allocator ownership encoding.
     (Allocator.Allocator storage allocator, Known ownership, Known encoding)
  => Stream ScalarValue
  -> Sequence storage allocator ownership 'Strictness.Lazy encoding
{-# INLINE unstreamLazy #-}
unstreamLazy (Stream step s0) = Sequence $ unstreamLazy' step s0

unstreamLazy' ::
     forall storage allocator ownership encoding s.
     (Allocator.Allocator storage allocator, Known ownership, Known encoding)
  => Streamer s
  -> s
  -> M.Sequence storage allocator ownership 'Strictness.Lazy (EF.CodeUnit encoding)
{-# INLINE unstreamLazy' #-}
unstreamLazy' step = loopStart SPEC
  where
    loopStart ::
         SPEC
      -> s
      -> M.Sequence storage allocator ownership 'Strictness.Lazy (EF.CodeUnit encoding)
    loopStart !sPEC s =
      case step s of
        Done -> M.nilL
        Skip s' -> loopStart sPEC s'
        Yield a s' -> loop1 sPEC a s'
    loop1 ::
         SPEC
      -> ScalarValue
      -> s
      -> M.Sequence storage allocator ownership 'Strictness.Lazy (EF.CodeUnit encoding)
    loop1 !sPEC !sv0 s = M.consChunk chunk rest
      where
        !(rest, chunk) =
          M.withAllocatorT $ do
            let chunkSize = inElements defaultChunkSize
            mutArr <- Allocator.new chunkSize
            let go1 !sPEC' offset s' sv =
                  EFI.genericEncode sv $ \diff write ->
                    let !newOffset = offset + diff
                     in if newOffset <= chunkSize
                          then do
                            write mutArr offset
                            go sPEC' newOffset s'
                          else pure (loop1 sPEC' sv s', (offset, mutArr))
                go !sPEC' offset s' =
                  case step s' of
                    Done -> pure (M.nilL, (offset, mutArr))
                    Skip s'' -> go sPEC' offset s''
                    Yield !sv s'' -> go1 sPEC' offset s'' sv
            go1 sPEC 0 s sv0

-- Note: Heterogeneous equality of lazy sequences likely slow.
equal ::
     ( Known storage1
     , Known ownership1
     , Known strictness1
     , Known storage2
     , Known ownership2
     , Known strictness2
     , Known encoding1
     , Known encoding2
     )
  => Sequence storage1 allocator1 ownership1 strictness1 encoding1
  -> Sequence storage2 allocator2 ownership2 strictness2 encoding2
  -> Bool
{-# INLINEABLE[0] equal #-}
equal x y =
  case encodingForm x `testEquality` encodingForm y of
    Just Refl -> representation x `M.equal` representation y
    Nothing -> stream x `equalStream` stream y

{-# RULES
"equal on representation"
  equal = \x y -> representation x `M.equal` representation y
"equal on streams"[1]
  equal = \x y -> stream x `equalStream` stream y
  #-}

equalStream :: Eq a => Stream a -> Stream a -> Bool
{-# INLINE equalStream #-}
equalStream (Stream stepX sX0) (Stream stepY sY0) = go SPEC sX0 sY0
 where
  go !sPEC sX !sY = case stepX sX of
    Yield x sX' -> goY sPEC x sX' sY
    Skip sX'    -> go sPEC sX' sY
    Done        -> yNull sPEC sY
  yNull !sPEC sY = case stepY sY of
    Yield _ _ -> False
    Done      -> True
    Skip sY'  -> yNull sPEC sY'
  goY !sPEC !x !sX sY = case stepY sY of
    Yield y sY' -> x == y && go sPEC sX sY'
    Skip sY'    -> goY sPEC x sX sY'
    Done        -> False

instance (Known storage, Known ownership, Known strictness, Known encoding) =>
         Eq (Sequence storage allocator ownership strictness encoding) where
  (==) = equal

toList ::
     (Known storage, Known ownership, Known strictness, Known encoding)
  => Sequence storage allocator ownership strictness encoding
  -> [ScalarValue]
{-# INLINE toList #-}
toList = streamToList . stream

fromList ::
     forall storage allocator ownership strictness encoding.
     ( Allocator.Allocator storage allocator
     , Known ownership
     , Known strictness
     , Known encoding
     )
  => [ScalarValue]
  -> Sequence storage allocator ownership strictness encoding
{-# INLINEABLE fromList #-}
fromList =
  case sing @strictness of
    Strictness.SStrict -> fromListStrict
    Strictness.SLazy -> fromListLazy

fromListStrict ::
     (Known encoding, Known ownership, Allocator.Allocator storage allocator)
  => [ScalarValue]
  -> Sequence storage allocator ownership 'Strictness.Strict encoding
{-# INLINEABLE fromListStrict #-}
fromListStrict svs = fromListNStrict (CountOf $ length svs) svs

fromListNStrict ::
     (Known encoding, Known ownership, Allocator.Allocator storage allocator)
  => CountOf ScalarValue
  -> [ScalarValue]
  -> Sequence storage allocator ownership 'Strictness.Strict encoding
{-# INLINE fromListNStrict #-}
fromListNStrict n svs =
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

fromListLazy ::
     (Allocator.Allocator storage allocator, Known ownership, Known encoding)
  => [ScalarValue]
  -> Sequence storage allocator ownership 'Strictness.Lazy encoding
{-# INLINEABLE fromListLazy #-}
fromListLazy = unstreamLazy . listToStream

instance ( Allocator.Allocator storage allocator
         , Known ownership
         , Known strictness
         , Known encoding
         ) =>
         E.IsList (Sequence storage allocator ownership strictness encoding) where
  type Item (Sequence storage allocator ownership strictness encoding) = ScalarValue
  toList = toList
  fromListN =
    case sing @strictness of
      Strictness.SStrict -> fromListNStrict . CountOf
      Strictness.SLazy -> const fromListLazy
  fromList = fromList

-- The default chunk size is chosen to be large enough that GHC RTS
-- pins individual chunks.  This way, a long forced lazy string will
-- not be copied in its entirety from heap to heap during collection.
defaultChunkSize :: ByteCount
defaultChunkSize = 4096
