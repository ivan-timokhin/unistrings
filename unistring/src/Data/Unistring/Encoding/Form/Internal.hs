{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Unistring.Encoding.Form.Internal
  ( codeUnitUpperBound
  , uncheckedDecode
  , genericEncode
  , don'tCheckCapacity
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Unistring.UCD.Unsafe (CodePoint(CodePoint))
import Data.Word (Word32)

import Data.Unistring.Encoding.Form
  ( CodeUnit(CU16, CU32, CU8)
  , EncodingForm(UTF16, UTF32, UTF8)
  , Sing(SUTF16, SUTF32, SUTF8)
  )
import Data.Unistring.Memory.Allocator.Unsafe (MutableArray(uncheckedWrite))
import Data.Unistring.Memory.Count (CountOf(CountOf))
import Data.Unistring.Memory.Internal.Readable (Readable(uncheckedRead))
import Data.Unistring.Scalar.Value.Unsafe (ScalarValue(ScalarValue))
import Data.Unistring.Singletons (Known(sing))

codeUnitUpperBound ::
     forall form. Known form
  => CountOf ScalarValue
  -> CountOf (CodeUnit form)
codeUnitUpperBound (CountOf n) =
  case sing @form of
    SUTF8 -> CountOf $ 4 * n
    SUTF16 -> CountOf $ 2 * n
    SUTF32 -> CountOf n

uncheckedDecode ::
     forall form arr m. (Known form, Readable m arr)
  => arr (CodeUnit form)
  -> CountOf (CodeUnit form)
  -> m (CountOf (CodeUnit form), ScalarValue)
{-# INLINE uncheckedDecode #-}
uncheckedDecode =
  case sing @form of
    SUTF8 -> uncheckedDecodeUTF8
    SUTF16 -> uncheckedDecodeUTF16
    SUTF32 -> uncheckedDecodeUTF32

uncheckedDecodeUTF8 ::
     Readable m arr
  => arr (CodeUnit 'UTF8)
  -> CountOf (CodeUnit 'UTF8)
  -> m (CountOf (CodeUnit 'UTF8), ScalarValue)
uncheckedDecodeUTF8 arr off = do
  CU8 b1 <- uncheckedRead arr off
  if | b1 .&. 0x80 == 0 -> pure (1, assumeSV $ fromIntegral b1)
     | b1 .&. 0xE0 == 0xC0 ->
       do CU8 b2 <- uncheckedRead arr $ off + 1
          pure
            ( 2
            , assumeSV $
              (fromIntegral b1 .&. 0x1F) `shiftL` 6 .|.
              (fromIntegral b2 .&. 0x3F))
     | b1 .&. 0xF0 == 0xE0 ->
       do CU8 b2 <- uncheckedRead arr $ off + 1
          CU8 b3 <- uncheckedRead arr $ off + 2
          let sv =
                (fromIntegral b1 .&. 0xF) `shiftL` 12 .|.
                (fromIntegral b2 .&. 0x3F) `shiftL` 6 .|.
                (fromIntegral b3 .&. 0x3F)
          pure (3, assumeSV sv)
     | otherwise ->
       do CU8 b2 <- uncheckedRead arr $ off + 1
          CU8 b3 <- uncheckedRead arr $ off + 2
          CU8 b4 <- uncheckedRead arr $ off + 3
          let sv =
                (fromIntegral b1 .&. 0x7) `shiftL` 18 .|.
                (fromIntegral b2 .&. 0x3F) `shiftL` 12 .|.
                (fromIntegral b3 .&. 0x3F) `shiftL` 6 .|.
                (fromIntegral b4 .&. 0x3F)
          pure (4, assumeSV sv)

uncheckedDecodeUTF16 ::
     Readable m arr
  => arr (CodeUnit 'UTF16)
  -> CountOf (CodeUnit 'UTF16)
  -> m (CountOf (CodeUnit 'UTF16), ScalarValue)
uncheckedDecodeUTF16 arr off = do
  CU16 w1 <- uncheckedRead arr off
  if w1 .&. 0xF800 == 0xD800
    then do
      CU16 w2 <- uncheckedRead arr $ off + 1
      let sv =
            ((fromIntegral w1 .&. 0x3FF) + 0x40) `shiftL` 10 .|.
            (fromIntegral w2 .&. 0x3FF)
      pure (2, assumeSV sv)
    else pure (1, assumeSV $ fromIntegral w1)

uncheckedDecodeUTF32 ::
     Readable m arr
  => arr (CodeUnit 'UTF32)
  -> CountOf (CodeUnit 'UTF32)
  -> m (CountOf (CodeUnit 'UTF32), ScalarValue)
uncheckedDecodeUTF32 arr off = do
  CU32 w <- uncheckedRead arr off
  pure (1, assumeSV w)

assumeSV :: Word32 -> ScalarValue
{-# INLINE assumeSV #-}
assumeSV = ScalarValue . CodePoint

genericEncode ::
     forall form arr m r. (Known form, MutableArray arr m)
  => ScalarValue
  -> (CountOf (CodeUnit form) -> (arr (CodeUnit form) -> CountOf (CodeUnit form) -> m ()) -> r)
  -> r
{-# INLINE genericEncode #-}
genericEncode =
  case sing @form of
    SUTF8 -> genericEncodeUTF8
    SUTF16 -> genericEncodeUTF16
    SUTF32 -> genericEncodeUTF32

genericEncodeUTF8 ::
     MutableArray arr m
  => ScalarValue
  -> (CountOf (CodeUnit 'UTF8) -> (arr (CodeUnit 'UTF8) -> CountOf (CodeUnit 'UTF8) -> m ()) -> r)
  -> r
{-# INLINE genericEncodeUTF8 #-}
genericEncodeUTF8 (ScalarValue (CodePoint sv)) withCapacity
  | sv .&. 0xFFFFFF80 == 0 =
    withCapacity 1 $ \arr offset ->
      uncheckedWrite arr offset $ CU8 $ fromIntegral sv
  | sv .&. 0xFFFFF800 == 0 =
    withCapacity 2 $ \arr offset -> do
      uncheckedWrite arr offset $ CU8 $ fromIntegral $ 0xC0 .|. (sv `shiftR` 6)
      uncheckedWrite arr (offset + 1) $
        CU8 $ fromIntegral $ 0x80 .|. (sv .&. 0x3F)
  | sv .&. 0xFFFF0000 == 0 =
    withCapacity 3 $ \arr offset -> do
      uncheckedWrite arr offset $ CU8 $ fromIntegral $ 0xE0 .|. (sv `shiftR` 12)
      uncheckedWrite arr (offset + 1) $
        CU8 $ fromIntegral $ 0x80 .|. ((sv `shiftR` 6) .&. 0x3F)
      uncheckedWrite arr (offset + 2) $
        CU8 $ fromIntegral $ 0x80 .|. (sv .&. 0x3F)
  | otherwise =
    withCapacity 4 $ \arr offset -> do
      uncheckedWrite arr offset $ CU8 $ fromIntegral $ 0xF0 .|. (sv `shiftR` 18)
      uncheckedWrite arr (offset + 1) $
        CU8 $ fromIntegral $ 0x80 .|. ((sv `shiftR` 12) .&. 0x3F)
      uncheckedWrite arr (offset + 2) $
        CU8 $ fromIntegral $ 0x80 .|. ((sv `shiftR` 6) .&. 0x3F)
      uncheckedWrite arr (offset + 3) $
        CU8 $ fromIntegral $ 0x80 .|. (sv .&. 0x3F)

genericEncodeUTF16 ::
     MutableArray arr m
  => ScalarValue
  -> (CountOf (CodeUnit 'UTF16) -> (arr (CodeUnit 'UTF16) -> CountOf (CodeUnit 'UTF16) -> m ()) -> r)
  -> r
{-# INLINE genericEncodeUTF16 #-}
genericEncodeUTF16 (ScalarValue (CodePoint sv)) withCapacity
  | sv < 0x10000 =
    withCapacity 1 $ \arr offset ->
      uncheckedWrite arr offset $ CU16 $ fromIntegral sv
  | otherwise =
    withCapacity 2 $ \arr offset -> do
      uncheckedWrite arr offset $
        CU16 $ fromIntegral $ 0xD800 .|. ((sv `shiftR` 10) - 0x40)
      uncheckedWrite arr (offset + 1) $
        CU16 $ fromIntegral $ 0xDC00 .|. (sv .&. 0x3FF)

genericEncodeUTF32 ::
     MutableArray arr m
  => ScalarValue
  -> (CountOf (CodeUnit 'UTF32) -> (arr (CodeUnit 'UTF32) -> CountOf (CodeUnit 'UTF32) -> m ()) -> r)
  -> r
{-# INLINE genericEncodeUTF32 #-}
genericEncodeUTF32 (ScalarValue (CodePoint sv)) withCapacity =
  withCapacity 1 $ \array offset -> uncheckedWrite array offset $ CU32 sv

don'tCheckCapacity ::
     CountOf (CodeUnit form)
  -> (arr (CodeUnit form) -> CountOf (CodeUnit form) -> m ())
  -> arr (CodeUnit form)
  -> CountOf (CodeUnit form)
  -> m ()
{-# INLINE don'tCheckCapacity #-}
don'tCheckCapacity _ = id
