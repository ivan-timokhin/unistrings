{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Data.Unistring.Memory.Internal.Readable
  ( Readable(uncheckedRead)
  ) where

import Foreign.Ptr (Ptr)

import Data.Unistring.Memory.Allocator.Unsafe (MonadWithPtr(uncheckedReadPtr))
import Data.Unistring.Memory.Array.Unsafe (NativeArray(NativeArray))
import Data.Unistring.Memory.Count (CountOf)
import Data.Unistring.Memory.Primitive.Class.Unsafe
  ( Primitive(uncheckedIndexBytes)
  )

class Monad m => Readable m arr where
  uncheckedRead :: Primitive a => arr a -> CountOf a -> m a

instance MonadWithPtr m => Readable m Ptr where
  uncheckedRead = uncheckedReadPtr

instance Monad m => Readable m NativeArray where
  uncheckedRead (NativeArray ba#) = pure . uncheckedIndexBytes ba#
