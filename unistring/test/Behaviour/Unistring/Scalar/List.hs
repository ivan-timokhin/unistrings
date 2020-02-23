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
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Behaviour.Unistring.Scalar.List
  ( ScalarList
  , getScalarList
  ) where

import Data.Unistring.UCD (toCodePoint)
import Test.Tasty.QuickCheck (Arbitrary)

import Data.Unistring.Scalar.Value (ScalarValue, checkScalarValue)

#if MIN_VERSION_tasty_quickcheck(0, 10, 1)
import Data.Maybe (fromMaybe)
import Test.Tasty.QuickCheck (UnicodeString(UnicodeString))

newtype ScalarList =
  ScalarList UnicodeString
  deriving (Eq, Show, Arbitrary)

getScalarList :: ScalarList -> [ScalarValue]
getScalarList (ScalarList (UnicodeString ustr)) =
  map
    (fromMaybe (error "UnicodeString generated a surrogate") .
     checkScalarValue . toCodePoint)
    ustr
#else
import Data.Maybe (mapMaybe)

newtype ScalarList =
  ScalarList String
  deriving (Eq, Show, Arbitrary)

getScalarList :: ScalarList -> [ScalarValue]
getScalarList (ScalarList str) = mapMaybe (checkScalarValue . toCodePoint) str
#endif
