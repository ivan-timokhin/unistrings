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
  , convert
  , append
  , Data.Unistring.Memory.Array.Internal.concat
  , times
  , empty
  , uncheckedCopyArray
  , Allocator(withAllocator, withAllocatorT, adopt)
  , AllocatorM(new, shrink, tryShrink)
  , MutableArray(uncheckedRead, uncheckedWrite,
             uncheckedCopyNativeSlice, uncheckedCopyForeignSlice)
  , MonadWithPtr(withForeignPtr, uncheckedReadPtr)
  , allocatorCoercion
  , forgetArrayAllocator
  ) where

import Control.Monad.ST (stToIO)
import Data.Foldable (for_, foldl')
import Data.Kind (Type)
import Data.Type.Coercion (Coercion(Coercion), coerceWith)
import qualified Foreign.ForeignPtr as ForeignPtr
import qualified GHC.Exts as E
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.IO (IO(IO))
import GHC.TypeLits (ErrorMessage((:$$:), (:<>:), ShowType, Text), TypeError)
import Data.Typeable (Typeable)
import Data.Type.Equality ((:~:)(Refl), testEquality)
import Control.Monad.Trans.State.Strict (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(Identity, runIdentity))

#if MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup(sconcat, stimes))
#else
import Data.Semigroup (Semigroup((<>), sconcat, stimes))
#endif

import Data.Unistring.Singletons (Known(sing))
import Data.Unistring.Compat.Typeable (TypeRep, typeRep)
import Data.Unistring.Memory.Count
  ( ByteCount(getByteCount)
  , CountOf(CountOf, getCountOf)
  )
import Data.Unistring.Memory.Primitive.Class.Unsafe (Primitive)
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as P
import qualified Data.Unistring.Memory.Primitive.Operations.Unsafe as Operations
import Data.Unistring.Memory.Storage
  ( Sing(SForeign, SNative)
  , Storage(Foreign, Native)
  )
import Data.Unistring.Internal.IO (readOnlyPerformIO, readWritePerformIO)

--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

data ForeignArray a =
  ForeignArray {-# UNPACK #-}!(ForeignPtr a) {-# UNPACK #-}!(CountOf a)

type role ForeignArray representational

foreignArrayPtr :: ForeignArray a -> ForeignPtr a
foreignArrayPtr (ForeignArray ptr _) = ptr

foreignArrayLength :: ForeignArray a -> CountOf a
{-# INLINE foreignArrayLength #-}
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
     Primitive a => NativeMutableArray E.RealWorld a -> CountOf a -> IO a
{-# INLINE uncheckedReadNative #-}
uncheckedReadNative (NativeMutableArray bytes#) =
  stToIO . P.uncheckedReadBytes bytes#

uncheckedWriteNative ::
     Primitive a => NativeMutableArray E.RealWorld a -> CountOf a -> a -> IO ()
{-# INLINE uncheckedWriteNative #-}
uncheckedWriteNative (NativeMutableArray bytes#) =
  (stToIO .) . P.uncheckedWriteBytes bytes#

nativeArrayLength :: Primitive a => NativeArray a -> CountOf a
{-# INLINE nativeArrayLength #-}
nativeArrayLength (NativeArray bytes#) =
  P.inElements (Operations.sizeOfByteArray bytes#)

getNativeMutableArrayLength ::
     Primitive a => NativeMutableArray E.RealWorld a -> IO (CountOf a)
{-# INLINE getNativeMutableArrayLength #-}
getNativeMutableArrayLength (NativeMutableArray bytes#) =
  P.inElements <$> Operations.getSizeOfMutableByteArray bytes#

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

instance (Allocator storage allocator, Primitive a) =>
         Semigroup (Array storage allocator a) where
  (<>) = append
  sconcat = Data.Unistring.Memory.Array.Internal.concat
  stimes = times

instance (Allocator storage allocator, Primitive a) =>
         Monoid (Array storage allocator a) where
  mempty = empty
#if !MIN_VERSION_base(4, 11, 0)
  mappend = (<>)
#endif
  mconcat = Data.Unistring.Memory.Array.Internal.concat

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
            P.uncheckedIndexBytes arr# (CountOf i)
    SForeign ->
      let !(FArray (ForeignArray fptr len)) = arr
       in flip map [0 .. getCountOf len - 1] $ \i ->
            readOnlyPerformIO $
            withForeignPtr fptr $ \ptr -> uncheckedReadPtr ptr (CountOf i)

size ::
     (Known storage, Primitive a) => Array storage alloc a -> CountOf a
{-# INLINE size #-}
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

nativeEq :: Array 'Native al a -> Array 'Native al' a -> Bool
{-# INLINABLE nativeEq #-}
nativeEq (NArray (NativeArray x#)) (NArray (NativeArray y#)) =
  let !xn = Operations.sizeOfByteArray x#
      !yn = Operations.sizeOfByteArray y#
  in  xn == yn && (Operations.compareBytesNative x# y# xn == 0)

foreignEq
  :: Primitive a => Array 'Foreign al a -> Array 'Foreign al' a -> Bool
{-# INLINABLE foreignEq #-}
foreignEq (FArray (ForeignArray xfptr xlen)) (FArray (ForeignArray yfptr ylen))
  = xlen == ylen && readOnlyPerformIO
    (withForeignPtr xfptr $ \xptr -> withForeignPtr yfptr $ \yptr ->
      (== 0)
        <$> Operations.compareBytesForeign xptr yptr (P.uncheckedInBytes xlen)
    )

mixedEq :: Primitive a => Array 'Native al a -> Array 'Foreign al' a -> Bool
{-# INLINABLE mixedEq #-}
mixedEq (NArray (NativeArray x#)) (FArray (ForeignArray yfptr ylen)) =
  let !xb = Operations.sizeOfByteArray x#
      !yb = P.uncheckedInBytes ylen
  in  xb == yb && readOnlyPerformIO
        ( withForeignPtr yfptr
        $ \yptr -> (== 0) <$> Operations.compareBytesMixed x# yptr xb
        )

convert ::
     ( Known storage
     , Typeable allocator
     , Allocator storage' allocator'
     , Primitive a
     )
  => Array storage allocator a
  -> Array storage' allocator' a
{-# INLINEABLE convert #-}
convert src =
  case adopt src of
    Just dest -> dest
    Nothing ->
      withAllocator $ do
        let !n = size src
        arr <- new n
        uncheckedCopyArray src arr 0
        pure arr

append ::
     ( Known xStorage
     , Known yStorage
     , Allocator zStorage zAllocator
     , Primitive a
     )
  => Array xStorage xAllocator a
  -> Array yStorage yAllocator a
  -> Array zStorage zAllocator a
{-# INLINEABLE append #-}
append x y =
  withAllocator $ do
    arr <- new $ size x + size y
    uncheckedCopyArray x arr 0
    uncheckedCopyArray y arr $ size x
    pure arr

concat ::
     ( Known storage
     , Typeable allocator
     , Allocator storage' allocator'
     , Primitive a
     , Foldable f
     )
  => f (Array storage allocator a)
  -> Array storage' allocator' a
{-# INLINEABLE concat #-}
concat arrays =
  withAllocator $ do
    result <- new $ foldl' (\n array -> n + size array) 0 arrays
    flip evalStateT 0 $
      for_ arrays $ \array -> do
        offset <- get
        lift $ uncheckedCopyArray array result offset
        put $! offset + size array
    pure result

times ::
     ( Known storage
     , Typeable allocator
     , Allocator storage' allocator'
     , Primitive a
     , Integral n)
  => n
  -> Array storage allocator a
  -> Array storage' allocator' a
{-# INLINEABLE times #-}
times 0 _ = empty
times 1 array = convert array
times n array
  | n < 0 = errorWithoutStackTrace "stimes Array: negative repetition count"
  | otherwise =
    withAllocator $ do
      let !sz = size array
      result <- new $ fromIntegral n * sz
      uncheckedCopyArray array result 0
      cyclePrefix result sz
      pure result

empty :: (Allocator storage allocator, Primitive a) => Array storage allocator a
{-# INLINEABLE empty #-}
empty = withAllocator $ new 0

uncheckedCopyArray ::
     (Known storage, Primitive a, MutableArray arr m, MonadWithPtr m)
  => Array storage allocator a
  -> arr a
  -> CountOf a
  -> m ()
{-# INLINE uncheckedCopyArray #-}
uncheckedCopyArray src dest destOff =
  case storage src of
    SNative ->
      let !(NArray (NativeArray src#)) = src
       in uncheckedCopyNativeSlice src# 0 dest destOff n
    SForeign ->
      let !(FArray (ForeignArray fptr _)) = src
       in withForeignPtr fptr $ \ptr ->
            uncheckedCopyForeignSlice ptr dest destOff n
  where
    !n = size src

--------------------------------------------------------------------------------
-- Allocators
--------------------------------------------------------------------------------

class Monad m =>
      MutableArray arr m
  where
  uncheckedRead :: Primitive a => arr a -> CountOf a -> m a
  uncheckedWrite :: Primitive a => arr a -> CountOf a -> a -> m ()
  uncheckedCopyNativeSlice ::
       Primitive a
    => E.ByteArray#
    -> CountOf a
    -> arr a
    -> CountOf a
    -> CountOf a
    -> m ()
  uncheckedCopyForeignSlice ::
       Primitive a => E.Ptr a -> arr a -> CountOf a -> CountOf a -> m ()
  uncheckedCopyArraySlice ::
       Primitive a
    => arr a
    -> CountOf a
    -> arr a
    -> CountOf a
    -> CountOf a
    -> m ()
  arraySize :: Primitive a => arr a -> m (CountOf a)

instance MutableArray (NativeMutableArray E.RealWorld) IO where
  uncheckedRead = uncheckedReadNative
  uncheckedWrite = uncheckedWriteNative
  uncheckedCopyNativeSlice src# srcOff (NativeMutableArray dest#) destOff n =
    Operations.copyNativeToNative
      src#
      (P.uncheckedInBytes srcOff)
      dest#
      (P.uncheckedInBytes destOff)
      (P.uncheckedInBytes n)
  uncheckedCopyForeignSlice src (NativeMutableArray dest#) destOff n =
    Operations.copyForeignToNative src dest# (P.inBytes destOff) (P.inBytes n)
  uncheckedCopyArraySlice (NativeMutableArray src#) srcOff (NativeMutableArray dest#) destOff n =
    Operations.copyMutableNativeToNative
      src#
      (P.uncheckedInBytes srcOff)
      dest#
      (P.uncheckedInBytes destOff)
      (P.uncheckedInBytes n)
  arraySize = getNativeMutableArrayLength

cyclePrefix :: (MutableArray arr m, Primitive a) => arr a -> CountOf a -> m ()
{-# INLINE cyclePrefix #-}
cyclePrefix arr prefixLen = do
  fullLen <- arraySize arr
  let go currentPrefixLen
        | currentPrefixLen >= fullLen = pure ()
        | let doubledLen = 2 * currentPrefixLen
        , doubledLen <= fullLen = do
          uncheckedCopyArraySlice arr 0 arr currentPrefixLen currentPrefixLen
          go doubledLen
        | otherwise =
          uncheckedCopyArraySlice arr 0 arr currentPrefixLen $
          fullLen - currentPrefixLen
  go prefixLen

class Monad m => MonadWithPtr m where
  withForeignPtr :: ForeignPtr a -> (E.Ptr a -> m r) -> m r
  uncheckedReadPtr :: Primitive a => E.Ptr a -> CountOf a -> m a

instance MonadWithPtr IO where
  withForeignPtr = ForeignPtr.withForeignPtr
  uncheckedReadPtr = P.uncheckedReadPtr

class (MutableArray arr m, MonadWithPtr m) =>
      AllocatorM arr m
  | m -> arr
  where
  new :: Primitive a => CountOf a -> m (arr a)
  tryShrink :: Primitive a => arr a -> CountOf a -> m (Maybe (arr a))
  tryShrink _ _ = pure Nothing
  shrink :: Primitive a => arr a -> CountOf a -> m (arr a)
  shrink arr n = do
    arr' <- new n
    uncheckedCopyArraySlice arr 0 arr' 0 n
    pure arr'

newtype AllocatorT alloc (arr :: Type -> Type) a =
  AllocatorT
    { runAllocatorT :: IO a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MutableArray (NativeMutableArray E.RealWorld)
           , MonadWithPtr
           )

data Default
data Pinned
data Unknown

instance AllocatorM (NativeMutableArray E.RealWorld) (AllocatorT Default (NativeMutableArray E.RealWorld)) where
  new n =
    AllocatorT $
    IO $ \s ->
      case E.newByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(E.I# byteCount) = getByteCount $ P.inBytes n
#if MIN_VERSION_base(4, 11, 0)
  shrink src@(NativeMutableArray src#) n =
    AllocatorT $ IO $ \s -> (# E.shrinkMutableByteArray# src# n# s, src #)
    where
      !(E.I# n#) = getByteCount $ P.inBytes n
  tryShrink arr n = Just <$> shrink arr n
#endif

instance AllocatorM (NativeMutableArray E.RealWorld) (AllocatorT Pinned (NativeMutableArray E.RealWorld)) where
  new n =
    AllocatorT $
    IO $ \s ->
      case E.newPinnedByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(E.I# byteCount) = getByteCount $ P.inBytes n

class (Known storage, Typeable alloc) =>
      Allocator (storage :: Storage) alloc
  where
  withAllocator ::
       Primitive a
    => (forall m arr. AllocatorM arr m =>
                        m (arr a))
    -> Array storage alloc a
  withAllocator body = runIdentity $ withAllocatorT (Identity <$> body)
  withAllocatorT ::
       (Primitive a, Traversable t)
    => (forall m arr. AllocatorM arr m =>
                        m (t (arr a)))
    -> t (Array storage alloc a)
  adopt ::
       (Typeable alloc', Known storage', Primitive a)
    => Array storage' alloc' a
    -> Maybe (Array storage alloc a)
  adopt arr = do
    Refl <- testEquality (allocator arr) (typeRep @alloc)
    Refl <- testEquality (storage arr) (sing @storage)
    Just arr

instance Allocator 'Native Default where
  withAllocatorT = withNativeAllocatorT run
    where
      run :: AllocatorT Default arr (t (arr a)) -> IO (t (arr a))
      run = runAllocatorT

instance Allocator 'Native Pinned where
  withAllocatorT = withNativeAllocatorT run
    where
      run :: AllocatorT Pinned arr (t (arr a)) -> IO (t (arr a))
      run = runAllocatorT
  adopt array
    | SNative <- storage array
    , isDefinitelyPinned array
    , (NArray (NativeArray ba#)) <- array
    = Just (NArray (NativeArray ba#))
    | otherwise = Nothing

withNativeAllocatorT ::
     Traversable t
  => (n (t (NativeMutableArray E.RealWorld a)) -> IO (t (NativeMutableArray E.RealWorld a)))
  -> n (t (NativeMutableArray E.RealWorld a))
  -> t (Array 'Native alloc a)
{-# INLINE withNativeAllocatorT #-}
withNativeAllocatorT run f =
  readWritePerformIO $ do
    mutArrs <- run f
    traverse (fmap NArray . unsafeFreezeNative) mutArrs

instance Allocator 'Foreign Pinned where
  withAllocatorT f =
    readWritePerformIO $ do
      mutArrs <- run f
      traverse (fmap FArray . unsafeFreezeNativeToForeign) mutArrs
    where
      run :: AllocatorT Pinned arr (t (arr a)) -> IO (t (arr a))
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
  withAllocatorT _ = error "unreachable"

instance (TypeError ('ShowType Unknown
                     ':<>: 'Text " is not an allocator, but a placeholder meaning that the actual allocator is not known;"
                     ':$$: 'Text "Use "
                     ':<>: 'ShowType Default
                     ':<>: 'Text " or "
                     ':<>: 'ShowType Pinned
                     ':<>: 'Text " instead")
         , Known storage) =>
         Allocator storage Unknown where
  withAllocatorT _ = error "unreachable"

unsafeFreezeNative :: NativeMutableArray E.RealWorld a -> IO (NativeArray a)
{-# INLINE unsafeFreezeNative #-}
unsafeFreezeNative (NativeMutableArray mbytes#) =
  IO $ \s ->
    case E.unsafeFreezeByteArray# mbytes# s of
      (# s', ba #) -> (# s', NativeArray ba #)

unsafeFreezeNativeToForeign ::
     Primitive a => NativeMutableArray E.RealWorld a -> IO (ForeignArray a)
{-# INLINE unsafeFreezeNativeToForeign #-}
unsafeFreezeNativeToForeign nma@(NativeMutableArray bytes#) = do
  n <- getNativeMutableArrayLength nma
  IO $ \s ->
    case E.unsafeFreezeByteArray# bytes# s of
      (# s', ba #) ->
        let fptr = ForeignPtr (E.byteArrayContents# ba) (PlainPtr bytes#)
         in (# s', ForeignArray fptr n #)
