module Data.UCD.Internal.Enum (toMEnum) where

toMEnum :: Enum a => Int -> Maybe a
{-# INLINE toMEnum #-}
toMEnum 0 = Nothing
toMEnum n = Just $ toEnum $ n - 1
