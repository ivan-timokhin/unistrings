{-
Copyright 2019 Ivan Timokhin

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
{-# LANGUAGE TypeFamilies #-}

module Gen.Mono where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Word (Word8)

class Monoid c =>
      Container c
  where
  type Elem c
  toList :: c -> [Elem c]
  isNull :: c -> Bool
  len :: c -> Int

instance Container B.ByteString where
  type Elem B.ByteString = Word8
  toList = B.unpack
  isNull = B.null
  len = B.length

instance Container (V.Vector a) where
  type Elem (V.Vector a) = a
  toList = V.toList
  isNull = V.null
  len = V.length
