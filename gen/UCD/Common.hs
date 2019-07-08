{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module UCD.Common where

import qualified Data.Vector as V
import Data.Word (Word32)

newtype Table annS annR a =
  Table
    { getTable :: [Range annS annR a]
    }
  deriving (Functor)

data Range annS annR a
  = Single Word32 annS a
  | Range Word32 Word32 annR a
  deriving (Eq, Show, Functor)

unicodeTableSize :: Int
unicodeTableSize = 0x110000

tableToVector :: a -> Table annS annR a -> V.Vector a
tableToVector def table = V.replicate unicodeTableSize def V.// assignments
  where
    assignments =
      getTable table >>= \case
        Single code _ udata -> [(fromIntegral code, udata)]
        Range lo hi _ udata -> [(fromIntegral i, udata) | i <- [lo .. hi]]
