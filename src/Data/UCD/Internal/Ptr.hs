{-# LANGUAGE MagicHash #-}

module Data.UCD.Internal.Ptr
  ( Ptr
  , unsafeReadPtr
  ) where

import GHC.Exts (Int(I#), Ptr(Ptr), indexIntOffAddr#)

class Readable a where
  unsafeReadPtr :: Ptr a -> Int -> a

instance Readable Int where
  unsafeReadPtr (Ptr addr) (I# offset) = I# (indexIntOffAddr# addr offset)
