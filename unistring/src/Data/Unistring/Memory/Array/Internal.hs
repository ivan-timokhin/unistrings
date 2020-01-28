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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Unistring.Memory.Array.Internal
  ( ForeignArray(ForeignArray)
  , foreignArrayPtr
  , foreignArrayLength
  , NativeArray(NativeArray)
  , nativeArrayBytes
  , nativeArrayLength
  , Default
  , Pinned
  , Unknown
  , Array(NArray, FArray, getNArray, getFArray)
  , storage
  , allocator
  , size
  , toList
  , equal
  , Allocator(withAllocator, adopt)
  , AllocatorM(new)
  , MutableArray(uncheckedRead, uncheckedWrite)
  , allocatorCoercion
  , forgetArrayAllocator
  ) where

import Control.Monad.ST (runST, stToIO)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Traversable (for)
import Data.Type.Coercion (Coercion(Coercion), coerceWith)
import Foreign.ForeignPtr (withForeignPtr)
import qualified GHC.Exts as E
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.IO (IO(IO))
import GHC.ST (ST(ST))
import GHC.TypeLits (ErrorMessage((:$$:), (:<>:), ShowType, Text), TypeError)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Typeable (Typeable)
import Data.Type.Equality ((:~:)(Refl), testEquality)

import Data.Unistring.Singletons (Known(sing))
import Data.Unistring.Compat.Typeable (TypeRep, typeRep)
import Data.Unistring.Memory.Count
  ( ByteCount(getByteCount)
  , CountOf(CountOf, getCountOf)
  )
import Data.Unistring.Memory.Primitive.Class.Unsafe
  ( Primitive(inBytes, inElements, uncheckedIndexBytes,
          uncheckedReadBytes, uncheckedReadPtr, uncheckedWriteBytes,
          uncheckedWritePtr)
  )
import Data.Unistring.Memory.Primitive.Operations.Unsafe
  ( compareBytesForeign
  , compareBytesMixed
  , compareBytesNative
  , getSizeOfMutableByteArray
  , sizeOfByteArray
  )
import Data.Unistring.Memory.Storage
  ( Sing(SForeign, SNative)
  , Storage(Foreign, Native)
  )

--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

data ForeignArray a =
  ForeignArray {-# UNPACK #-}!(ForeignPtr a) {-# UNPACK #-}!(CountOf a)

type role ForeignArray representational

foreignArrayPtr :: ForeignArray a -> ForeignPtr a
foreignArrayPtr (ForeignArray ptr _) = ptr

foreignArrayLength :: ForeignArray a -> CountOf a
foreignArrayLength (ForeignArray _ len) = len

data NativeArray a =
  NativeArray E.ByteArray#

type role NativeArray representational

nativeArrayBytes :: NativeArray a -> E.ByteArray#
nativeArrayBytes (NativeArray ba#) = ba#

data NativeMutableArray s a =
  NativeMutableArray (E.MutableByteArray# s)

type role NativeMutableArray nominal representational

uncheckedReadNative ::
     Primitive a => NativeMutableArray s a -> CountOf a -> ST s a
{-# INLINE uncheckedReadNative #-}
uncheckedReadNative (NativeMutableArray bytes#) = uncheckedReadBytes bytes#

uncheckedWriteNative ::
     Primitive a => NativeMutableArray s a -> CountOf a -> a -> ST s ()
{-# INLINE uncheckedWriteNative #-}
uncheckedWriteNative (NativeMutableArray bytes#) = uncheckedWriteBytes bytes#

nativeArrayLength :: Primitive a => NativeArray a -> CountOf a
{-# INLINE nativeArrayLength #-}
nativeArrayLength (NativeArray bytes#) = inElements (sizeOfByteArray bytes#)

getNativeMutableArrayLength ::
     Primitive a => NativeMutableArray s a -> ST s (CountOf a)
{-# INLINE getNativeMutableArrayLength #-}
getNativeMutableArrayLength (NativeMutableArray bytes#) =
  inElements <$> getSizeOfMutableByteArray bytes#

class Monad m =>
      MutableArray arr m
  where
  uncheckedRead :: Primitive a => arr a -> CountOf a -> m a
  uncheckedWrite :: Primitive a => arr a -> CountOf a -> a -> m ()

instance MutableArray E.Ptr IO where
  uncheckedRead = uncheckedReadPtr
  uncheckedWrite = uncheckedWritePtr

instance MutableArray (NativeMutableArray s) (ST s) where
  uncheckedRead = uncheckedReadNative
  uncheckedWrite = uncheckedWriteNative

instance MutableArray (NativeMutableArray E.RealWorld) IO where
  uncheckedRead arr = stToIO . uncheckedReadNative arr
  uncheckedWrite arr ix = stToIO . uncheckedWriteNative arr ix

data family Array (storage :: Storage) allocator :: Type -> Type

newtype instance Array 'Native allocator a =
  NArray {getNArray :: NativeArray a}

newtype instance Array 'Foreign allocator a =
  FArray {getFArray :: ForeignArray a}

instance (Known storage, Primitive a) => Eq (Array storage allocator a) where
  (==) = equal

instance (Known storage, Primitive a, Show a) =>
         Show (Array storage allocator a) where
  showsPrec p = showsPrec p . toList

-- Allowed to have false negatives, but not false positives
isDefinitelyPinned :: Typeable allocator => Array 'Native allocator a -> Bool
{-# INLINE isDefinitelyPinned #-}
#if MIN_VERSION_base(4, 10, 0)
-- On GHC >= 8.2/base >= 4.10, there's a isByteArrayPinned# primop,
-- through which we can just ask the RTS if the array is pinned.
isDefinitelyPinned (NArray (NativeArray ba#)) =
  E.isTrue# (E.isByteArrayPinned# ba#)
#else
-- On GHC 8.0/base 4.9, the primop is absent, so we resort to the next
-- best thing—seeing if the array was allocated via Pinned allocator.
isDefinitelyPinned array =
  -- Not using isJust, because it's used nowhere else in this module,
  -- so I would have to add a CPP'd import, which is one thing I
  -- like less than CPP'd code.
  case testEquality (allocator array) (typeRep @Pinned) of
    Just Refl -> True
    Nothing -> False
#endif

allocator :: Typeable alloc => Array storage alloc a -> TypeRep alloc
{-# INLINE allocator #-}
allocator = const typeRep

storage :: Known storage => Array storage alloc a -> Sing storage
{-# INLINE storage #-}
storage = const sing

instance (Allocator storage alloc, Primitive a) =>
         E.IsList (Array storage alloc a) where
  type Item (Array storage alloc a) = a
  fromList xs = E.fromListN (length xs) xs
  fromListN n xs =
    withAllocator $ do
      arr <- new (CountOf n)
      for_ (zip [0 ..] xs) $ uncurry (uncheckedWrite arr)
      pure arr
  {-# INLINEABLE fromListN #-}
  toList = toList
  {-# INLINE toList #-}

toList :: (Known storage, Primitive a) => Array storage alloc a -> [a]
{-# INLINE toList #-}
toList arr =
  case storage arr of
    SNative ->
      let !(NArray (NativeArray arr#)) = arr
       in flip map [0 .. (getCountOf $ nativeArrayLength (getNArray arr) - 1)] $ \i ->
            uncheckedIndexBytes arr# (CountOf i)
    SForeign ->
      unsafeDupablePerformIO $
      withForeignPtr (foreignArrayPtr (getFArray arr)) $ \ptr ->
        for [0 .. foreignArrayLength (getFArray arr) - 1] $ \i ->
          uncheckedReadPtr ptr i

size ::
     (Known storage, Primitive a) => Array storage alloc a -> CountOf a
{-# INLINEABLE size #-}
size arr =
  case storage arr of
    SNative -> nativeArrayLength (getNArray arr)
    SForeign -> foreignArrayLength (getFArray arr)

allocatorCoercion ::
     forall alloc1 alloc2 storage a. Known storage
  => Coercion (Array storage alloc1 a) (Array storage alloc2 a)
{-# INLINE allocatorCoercion #-}
allocatorCoercion =
  case sing @storage of
    SNative -> Coercion
    SForeign -> Coercion

forgetArrayAllocator ::
     forall alloc storage a. Known storage
  => Array storage alloc a
  -> Array storage Unknown a
{-# INLINE forgetArrayAllocator #-}
forgetArrayAllocator =
  coerceWith (allocatorCoercion @alloc @Unknown @storage @a)

equal ::
     (Known storage1, Known storage2, Primitive a)
  => Array storage1 alloc1 a
  -> Array storage2 alloc2 a
  -> Bool
{-# INLINEABLE equal #-}
equal x y =
  case (storage x, storage y) of
    (SNative, SNative) -> nativeEq (getNArray x) (getNArray y)
    (SForeign, SForeign) -> foreignEq (getFArray x) (getFArray y)
    (SNative, SForeign) -> mixedEq (getNArray x) (getFArray y)
    (SForeign, SNative) -> mixedEq (getNArray y) (getFArray x)
  where
    nativeEq :: NativeArray a -> NativeArray a -> Bool
    {-# INLINE nativeEq #-}
    nativeEq x' y' =
      let !(NativeArray x#) = x'
          !(NativeArray y#) = y'
          !xn = sizeOfByteArray x#
          !yn = sizeOfByteArray y#
       in xn == yn && (compareBytesNative x# y# xn == 0)
    foreignEq :: Primitive a => ForeignArray a -> ForeignArray a -> Bool
    {-# INLINE foreignEq #-}
    foreignEq x' y' =
      let !(ForeignArray xfptr xlen) = x'
          !(ForeignArray yfptr ylen) = y'
       in xlen == ylen &&
          unsafeDupablePerformIO
            (withForeignPtr xfptr $ \xptr ->
               withForeignPtr yfptr $ \yptr ->
                 (== 0) <$> compareBytesForeign xptr yptr (inBytes xlen))
    mixedEq :: Primitive a => NativeArray a -> ForeignArray a -> Bool
    {-# INLINE mixedEq #-}
    mixedEq x' y' =
      let !(NativeArray x#) = x'
          !(ForeignArray yfptr ylen) = y'
          !xb = sizeOfByteArray x#
          !yb = inBytes ylen
       in xb == yb &&
          unsafeDupablePerformIO
            (withForeignPtr yfptr $ \yptr ->
               (== 0) <$> compareBytesMixed x# yptr xb)

--------------------------------------------------------------------------------
-- Allocators
--------------------------------------------------------------------------------

class MutableArray arr m => AllocatorM arr m | m -> arr where
  new :: Primitive a => CountOf a -> m (arr a)

newtype AllocatorT alloc (arr :: Type -> Type) m a =
  AllocatorT
    { runAllocatorT :: m a
    }
  deriving (Functor, Applicative, Monad, MutableArray arr)

data Default
data Pinned
data Unknown

instance AllocatorM (NativeMutableArray s) (AllocatorT Default (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case E.newByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(E.I# byteCount) = getByteCount $ inBytes n

instance AllocatorM (NativeMutableArray s) (AllocatorT Pinned (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case E.newPinnedByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(E.I# byteCount) = getByteCount $ inBytes n

instance AllocatorM (NativeMutableArray E.RealWorld) (AllocatorT Pinned (NativeMutableArray E.RealWorld) IO) where
  new = cast . new
    where
      cast :: AllocatorT s a (ST E.RealWorld) b -> AllocatorT s a IO b
      cast = AllocatorT . stToIO . runAllocatorT

class (Known storage, Typeable alloc) =>
      Allocator (storage :: Storage) alloc
  where
  withAllocator ::
       Primitive a
    => (forall m arr. AllocatorM arr m =>
                        m (arr a))
    -> Array storage alloc a
  adopt ::
       (Typeable alloc', Known storage', Primitive a)
    => Array storage' alloc' a
    -> Maybe (Array storage alloc a)
  adopt arr = do
    Refl <- testEquality (allocator arr) (typeRep @alloc)
    Refl <- testEquality (storage arr) (sing @storage)
    Just arr

instance Allocator 'Native Default where
  withAllocator f = runST $ withNativeAllocator run f
    where
      run :: AllocatorT Default arr m (arr a) -> m (arr a)
      run = runAllocatorT

instance Allocator 'Native Pinned where
  withAllocator f = runST $ withNativeAllocator run f
    where
      run :: AllocatorT Pinned arr m (arr a) -> m (arr a)
      run = runAllocatorT
  adopt array
    | SNative <- storage array
    , isDefinitelyPinned array
    , (NArray (NativeArray ba#)) <- array
    = Just (NArray (NativeArray ba#))
    | otherwise = Nothing

withNativeAllocator ::
     AllocatorM (NativeMutableArray s) n
  => (n (NativeMutableArray s a) -> ST s (NativeMutableArray s a))
  -> n (NativeMutableArray s a)
  -> ST s (Array 'Native alloc a)
{-# INLINE withNativeAllocator #-}
withNativeAllocator run f = do
  mutArr <- run f
  arr <- unsafeFreezeNative mutArr
  pure $ NArray arr

instance Allocator 'Foreign Pinned where
  withAllocator f =
    unsafeDupablePerformIO $ do
      mutArr <- run f
      arr <- unsafeFreezeNativeToForeign mutArr
      pure $ FArray arr
    where
      run :: AllocatorT Pinned arr m (arr a) -> m (arr a)
      run = runAllocatorT
  adopt array =
    case storage array of
      SForeign ->
        case testEquality (allocator array) (typeRep @Pinned) of
          Just Refl -> Just array
          Nothing -> Nothing
      SNative
        | (NArray narr@(NativeArray ba#)) <- array
        , isDefinitelyPinned array ->
          Just $
          FArray $
          ForeignArray
            (ForeignPtr (E.byteArrayContents# ba#) (PlainPtr (E.unsafeCoerce# ba#)))
            (nativeArrayLength narr)
            -- mallocForeignPtrBytes in GHC.ForeignPtr has exactly the
            -- same unsafeCoerce#, but in opposite direction, so I
            -- assume this is safe-ish.  Especially since nobody's
            -- going to /access/ this byte array, it's just there to
            -- keep it alive.
        | otherwise -> Nothing

instance TypeError ('ShowType Default
                    ':<>: 'Text " cannot be used to allocate "
                    ':<>: 'ShowType 'Foreign
                    ':<>: 'Text " arrays;"
                    ':$$: 'Text "Use "
                    ':<>: 'ShowType Pinned
                    ':<>: 'Text " instead") =>
         Allocator 'Foreign Default where
  withAllocator _ = error "unreachable"

instance (TypeError ('ShowType Unknown
                     ':<>: 'Text " is not an allocator, but a placeholder meaning that the actual allocator is not known;"
                     ':$$: 'Text "Use "
                     ':<>: 'ShowType Default
                     ':<>: 'Text " or "
                     ':<>: 'ShowType Pinned
                     ':<>: 'Text " instead")
         , Known storage) =>
         Allocator storage Unknown where
  withAllocator _ = error "unreachable"

unsafeFreezeNative :: NativeMutableArray s a -> ST s (NativeArray a)
{-# INLINE unsafeFreezeNative #-}
unsafeFreezeNative (NativeMutableArray mbytes#) =
  ST $ \s ->
    case E.unsafeFreezeByteArray# mbytes# s of
      (# s', ba #) -> (# s', NativeArray ba #)

unsafeFreezeNativeToForeign ::
     Primitive a => NativeMutableArray E.RealWorld a -> IO (ForeignArray a)
{-# INLINE unsafeFreezeNativeToForeign #-}
unsafeFreezeNativeToForeign nma@(NativeMutableArray bytes#) = do
  n <- stToIO $ getNativeMutableArrayLength nma
  IO $ \s ->
    case E.unsafeFreezeByteArray# bytes# s of
      (# s', ba #) ->
        let fptr = ForeignPtr (E.byteArrayContents# ba) (PlainPtr bytes#)
         in (# s', ForeignArray fptr n #)
