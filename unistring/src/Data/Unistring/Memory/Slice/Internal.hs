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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Unistring.Memory.Slice.Internal
  ( Slice(NativeSlice, ForeignSlice)
  , storage
  , allocator
  , fromArray
  , toArray
  , convert
  , equal
  , Data.Unistring.Memory.Slice.Internal.splitAt
  , sliceUnchecked
  , size
  , uncons
  , toList
  ) where

import qualified GHC.Exts as E
import GHC.ForeignPtr (ForeignPtr(ForeignPtr))
import Foreign.ForeignPtr (withForeignPtr)
import Data.List (unfoldr)
import Data.Type.Equality ((:~:)(Refl), testEquality)

import qualified Data.Unistring.Memory.Storage as Storage
import Data.Unistring.Memory.Storage (Storage, Sing(SNative, SForeign))
import Data.Unistring.Memory.Count (CountOf, ByteCount(ByteCount))
import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Allocator.Unsafe as Allocator
import Data.Unistring.Memory.Array (Array)
import Data.Unistring.Singletons (Known(sing))
import Data.Unistring.Memory.Primitive.Class.Unsafe
  ( Primitive(uncheckedInBytes, uncheckedIndexBytes, uncheckedReadPtr)
  )
import Data.Unistring.Compat.Typeable (TypeRep, typeRep, Typeable)
import Data.Unistring.Memory.Primitive.Operations.Unsafe
  ( compareBytesSliceForeign
  , compareBytesSliceMixed
  , compareBytesSliceNative
  )
import Data.Unistring.Internal.IO (readOnlyPerformIO)

data family Slice (storage :: Storage) allocator a

newtype instance Slice 'Storage.Foreign allocator a =
  ForeignSlice (Array 'Storage.Foreign allocator a)

data instance Slice 'Storage.Native allocator a =
  NativeSlice
    {-# UNPACK #-}!(Array 'Storage.Native allocator a)
    {-# UNPACK #-}!(CountOf a)
    {-# UNPACK #-}!(CountOf a)

instance (Known storage, Primitive a) => Eq (Slice storage allocator a) where
  (==) = equal

instance (Known storage, Primitive a, Show a) =>
         Show (Slice storage allocator a) where
  showsPrec p = showsPrec p . toList

storage :: Known storage => Slice storage allocator a -> Sing storage
{-# INLINE storage #-}
storage = const sing

allocator ::
     Typeable allocator => Slice storage allocator a -> TypeRep allocator
{-# INLINE allocator #-}
allocator = const typeRep

equal ::
     (Known storage1, Known storage2, Primitive a)
  => Slice storage1 allocator1 a
  -> Slice storage2 allocator2 a
  -> Bool
{-# INLINEABLE[0] equal #-}
equal x y =
  case (storage x, storage y) of
    (SNative, SNative) -> nativeEq x y
    (SForeign, SForeign) -> foreignEq x y
    (SNative, SForeign) -> mixedEq x y
    (SForeign, SNative) -> mixedEq y x

{-# RULES
"equal -> nativeEq"     equal = nativeEq
"equal -> foreignEq"    equal = foreignEq
"equal -> mixedEq"      equal = mixedEq
"equal -> flip mixedEq" equal = flip mixedEq
  #-}

nativeEq ::
     Primitive a
  => Slice 'Storage.Native allocator1 a
  -> Slice 'Storage.Native allocator2 a
  -> Bool
{-# INLINE nativeEq #-}
nativeEq
  (NativeSlice (Array.NArray (Array.NativeArray x#)) xoff xlen)
  (NativeSlice (Array.NArray (Array.NativeArray y#)) yoff ylen)
  = xlen == ylen
  && compareBytesSliceNative x# (uncheckedInBytes xoff)
                             y# (uncheckedInBytes yoff)
                             (uncheckedInBytes xlen) == 0

foreignEq ::
     Primitive a
  => Slice 'Storage.Foreign allocator1 a
  -> Slice 'Storage.Foreign allocator2 a
  -> Bool
{-# INLINE foreignEq #-}
foreignEq
  (ForeignSlice (Array.FArray (Array.ForeignArray xfptr xlen)))
  (ForeignSlice (Array.FArray (Array.ForeignArray yfptr ylen)))
  = xlen == ylen
  && readOnlyPerformIO
       (withForeignPtr xfptr $ \xptr ->
          withForeignPtr yfptr $ \yptr ->
            (== 0) <$> compareBytesSliceForeign xptr yptr (uncheckedInBytes xlen))

mixedEq ::
     Primitive a
  => Slice 'Storage.Native allocator1 a
  -> Slice 'Storage.Foreign allocator2 a
  -> Bool
{-# INLINE mixedEq #-}
mixedEq
  (NativeSlice (Array.NArray (Array.NativeArray x#)) xoff xlen)
  (ForeignSlice (Array.FArray (Array.ForeignArray yfptr ylen)))
  = xlen == ylen
  && readOnlyPerformIO
       (withForeignPtr yfptr $ \yptr ->
          (== 0) <$> compareBytesSliceMixed x# (uncheckedInBytes xoff) yptr (uncheckedInBytes xlen))

fromArray ::
     (Primitive a, Known storage)
  => Array storage allocator a
  -> Slice storage allocator a
{-# INLINEABLE fromArray #-}
fromArray arr =
  case Array.storage arr of
    SForeign -> ForeignSlice arr
    SNative -> NativeSlice arr 0 (Array.size arr)

toArray ::
     ( Primitive a
     , Known storage
     , Typeable allocator
     , Allocator.Allocator storage' allocator'
     )
  => Slice storage allocator a
  -> Array storage' allocator' a
{-# INLINEABLE toArray #-}
toArray slice =
  case storage slice of
    SNative ->
      let !(NativeSlice arr@(Array.NArray (Array.NativeArray arr#)) xoff xlen) =
            slice
       in case xlen == Array.size arr of
            True
              | Just arr' <- Allocator.adopt arr -> arr'
            _ ->
              Allocator.withAllocator $ do
                arr' <- Allocator.new xlen
                Allocator.uncheckedCopyNativeSlice arr# xoff arr' 0 xlen
                pure arr'
    SForeign ->
      let !(ForeignSlice (Array.FArray (Array.ForeignArray xfptr xlen))) = slice
       in Allocator.withAllocator $ do
            arr' <- Allocator.new xlen
            Allocator.withForeignPtr xfptr $ \xptr ->
              Allocator.uncheckedCopyForeignSlice xptr arr' 0 xlen
            pure arr'

convert ::
  forall storage' allocator' storage allocator a.
     ( Primitive a
     , Known storage
     , Typeable allocator
     , Allocator.Allocator storage' allocator'
     )
  => Slice storage allocator a
  -> Slice storage' allocator' a
{-# INLINEABLE convert #-}
convert slice =
  case storage slice of
    SNative
      | (NativeSlice arr xoff xlen) <- slice
      , Just arr' <- Allocator.adopt arr ->
        sliceUnchecked xoff xlen $ fromArray arr'
    SForeign
      | SForeign <- sing @storage'
      , Just Refl <- testEquality (allocator slice) (typeRep @allocator') ->
        slice
    _ -> fromArray $ toArray slice -- TODO: this may re-check `adopt`

splitAt ::
     (Known storage, Primitive a)
  => CountOf a
  -> Slice storage allocator a
  -> (Slice storage allocator a, Slice storage allocator a)
{-# INLINEABLE splitAt #-}
splitAt n sl
  | n < sz = (sliceUnchecked 0 n sl, sliceUnchecked n (sz - n) sl)
  | n <= 0 = (sliceUnchecked 0 0 sl, sl)
  | otherwise = (sl, sliceUnchecked sz 0 sl)
  where
    !sz = size sl

sliceUnchecked ::
     (Known storage, Primitive a)
  => CountOf a
  -> CountOf a
  -> Slice storage allocator a
  -> Slice storage allocator a
{-# INLINEABLE sliceUnchecked #-}
sliceUnchecked newOffset newSize slice =
  case storage slice of
    SForeign ->
      let ForeignSlice (Array.FArray (Array.ForeignArray fptr _)) = slice
       in ForeignSlice $
          Array.FArray $
          Array.ForeignArray (fptr `plusForeignPtr'` newOffset) newSize
    SNative ->
      let NativeSlice array oldOffset _ = slice
       in NativeSlice array (oldOffset + newOffset) newSize

plusForeignPtr :: ForeignPtr a -> ByteCount -> ForeignPtr a
{-# INLINE plusForeignPtr #-}
plusForeignPtr (ForeignPtr addr# finalisers) (ByteCount (E.I# diff#)) =
  ForeignPtr (E.plusAddr# addr# diff#) finalisers

plusForeignPtr' :: Primitive a => ForeignPtr a -> CountOf a -> ForeignPtr a
{-# INLINE plusForeignPtr' #-}
plusForeignPtr' fptr diff = fptr `plusForeignPtr` uncheckedInBytes diff

size :: Known storage => Slice storage allocator a -> CountOf a
{-# INLINEABLE size #-}
size slice =
  case storage slice of
    SNative ->
      let (NativeSlice _ _ len) = slice
       in len
    SForeign ->
      let ForeignSlice (Array.FArray (Array.ForeignArray _ len)) = slice
       in len

uncons ::
     (Known storage, Primitive a)
  => Slice storage allocator a
  -> Maybe (a, Slice storage allocator a)
{-# INLINEABLE uncons #-}
uncons slice =
  case storage slice of
    SForeign ->
      let ForeignSlice (Array.FArray (Array.ForeignArray fptr len)) = slice
       in if len == 0
            then Nothing
            else Just
                   ( readOnlyPerformIO $
                     withForeignPtr fptr $ \ptr -> uncheckedReadPtr ptr 0
                   , ForeignSlice
                       (Array.FArray
                          (Array.ForeignArray
                             (fptr `plusForeignPtr'` (1 `asTypeOf` len))
                             (len - 1))))
    SNative ->
      let !(NativeSlice (Array.NArray (Array.NativeArray ba#)) offset len) =
            slice
       in if len == 0
            then Nothing
            else Just
                   ( uncheckedIndexBytes ba# offset
                   , NativeSlice
                       (Array.NArray (Array.NativeArray ba#))
                       (offset + 1)
                       (len - 1))

toList :: (Known storage, Primitive a) => Slice storage allocator a -> [a]
{-# INLINE toList #-}
toList = unfoldr uncons

instance (Allocator.Allocator storage allocator, Primitive a) =>
         E.IsList (Slice storage allocator a) where
  type Item (Slice storage allocator a) = a
  fromList xs = E.fromListN (length xs) xs
  fromListN n xs = fromArray $ E.fromListN n xs
  toList = toList
