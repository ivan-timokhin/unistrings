{-# LANGUAGE FlexibleContexts #-}

module Gen.Cost
  ( SizedTy(sizeInBytes)
  ) where

class SizedTy ty where
  sizeInBytes :: ty -> Int -> Int
