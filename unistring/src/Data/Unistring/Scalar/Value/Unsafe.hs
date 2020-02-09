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
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Scalar.Value.Unsafe
Description : Unicode scalar value type—unsafe interface
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Scalar.Value.Unsafe
  ( ScalarValue(ScalarValue)
  , checkScalarValue
  ) where

import Data.Unistring.UCD (CodePoint, IsCodePoint(toCodePoint))
import Data.Unistring.UCD.Unsafe (getCodePoint)

-- | Unicode scalar value, according to the definition D76 of the
-- Unicode Standard, is ‘any Unicode code-point, except high-surrogate
-- and low-surrogate code points.’
newtype ScalarValue =
  ScalarValue CodePoint
  deriving (Eq, Ord, Show)

instance IsCodePoint ScalarValue where
  toCodePoint (ScalarValue cp) = cp

checkScalarValue :: CodePoint -> Maybe ScalarValue
checkScalarValue cp
  | 0xD800 <= wcp && wcp < 0xE000 = Nothing
  | otherwise = Just (ScalarValue cp)
  where
    wcp = getCodePoint cp
