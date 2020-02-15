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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Encoding.Form
Description : Unicode encoding forms
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Encoding.Form
  ( EncodingForm(UTF8, UTF16, UTF32)
  , Sing(SUTF8, SUTF16, SUTF32)
  , CodeUnit(CU8, CU16, CU32)
  ) where

import Data.Type.Equality ((:~:)(Refl))
import Data.Word (Word16, Word32, Word8)

import Data.Unistring.Memory.Primitive.Class.Unsafe (Primitive)
import Data.Unistring.Singletons
  ( Decided(Disproven, Proven)
  , Known(sing)
  , SDecide(decideEq)
  , Sing
  )

data EncodingForm
  = UTF8
  | UTF16
  | UTF32

data instance  Sing (form :: EncodingForm) where
        SUTF8 :: Sing 'UTF8
        SUTF16 :: Sing 'UTF16
        SUTF32 :: Sing 'UTF32

deriving instance Show (Sing (form :: EncodingForm))

instance Known 'UTF8 where
  sing = SUTF8

instance Known 'UTF16 where
  sing = SUTF16

instance Known 'UTF32 where
  sing = SUTF32

instance SDecide EncodingForm where
  decideEq SUTF8 SUTF8 = Proven Refl
  decideEq SUTF16 SUTF16 = Proven Refl
  decideEq SUTF32 SUTF32 = Proven Refl
  decideEq SUTF8 SUTF16 = Disproven $ \case {}
  decideEq SUTF8 SUTF32 = Disproven $ \case {}
  decideEq SUTF16 SUTF8 = Disproven $ \case {}
  decideEq SUTF16 SUTF32 = Disproven $ \case {}
  decideEq SUTF32 SUTF8 = Disproven $ \case {}
  decideEq SUTF32 SUTF16 = Disproven $ \case {}

data family CodeUnit (form :: EncodingForm)

newtype instance  CodeUnit 'UTF8 = CU8 Word8

newtype instance  CodeUnit 'UTF16 = CU16 Word16

newtype instance  CodeUnit 'UTF32 = CU32 Word32

deriving instance Primitive (CodeUnit 'UTF8)

deriving instance Primitive (CodeUnit 'UTF16)

deriving instance Primitive (CodeUnit 'UTF32)
