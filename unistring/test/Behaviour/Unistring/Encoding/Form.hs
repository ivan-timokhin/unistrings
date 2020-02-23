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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Behaviour.Unistring.Encoding.Form
  ( SomeEncodingForm(SomeEncodingForm)
  ) where

import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), choose)

import Data.Unistring.Encoding.Form (EncodingForm, Sing(SUTF8, SUTF16, SUTF32))
import Data.Unistring.Singletons (Known)

data SomeEncodingForm where
  SomeEncodingForm
    :: Known encodingForm
    => Sing (encodingForm :: EncodingForm)
    -> SomeEncodingForm

deriving instance Show SomeEncodingForm

instance Arbitrary SomeEncodingForm where
  arbitrary = do
    n <- choose (0 :: Int, 2)
    pure $
      case n of
        0 -> SomeEncodingForm SUTF8
        1 -> SomeEncodingForm SUTF16
        _ -> SomeEncodingForm SUTF32
  shrink (SomeEncodingForm ef) =
    case ef of
      SUTF32 -> []
      _ -> [SomeEncodingForm SUTF32]
