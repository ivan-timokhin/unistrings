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
{-# LANGUAGE OverloadedStrings #-}

module Driver.UCD
  ( processTableAs
  , generateASCIITableSources
  , generateASCIIVectorTableSources
  , generateSourcesAs
  , generateTests
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import Driver
  ( Locate(Locate, cSymbolName, moduleName, modulePath, path)
  , TableValue
  , generateASCIITableSourcesAt
  , generateASCIIVectorTableSourcesAt
  , generateSourcesAtAs
  , generateTestsAt
  , processTableAtAs
  )
import Gen.Cost (SizedTy)

processTableAs ::
     (Show a, Ord bv, Ord (dv bv), SizedTy bt, VG.Vector sv a, VG.Vector dv bv)
  => TableValue sv dv a bv bt
  -> (Int, Int)
  -> ByteString
  -> sv a
  -> IO ()
{-# INLINE processTableAs #-}
processTableAs = processTableAtAs ucdLocate

generateASCIITableSources ::
     (Int, Int) -> ByteString -> V.Vector ByteString -> IO ()
generateASCIITableSources = generateASCIITableSourcesAt ucdLocate

-- This assumes that the total length of all strings in a single
-- element is <= 255
generateASCIIVectorTableSources ::
     (Int, Int) -> ByteString -> V.Vector (V.Vector ByteString) -> IO ()
generateASCIIVectorTableSources = generateASCIIVectorTableSourcesAt ucdLocate

generateSourcesAs ::
     (Ord (dv bv), Ord bv, SizedTy bt, VG.Vector dv bv, VG.Vector sv a)
  => TableValue sv dv a bv bt
  -> (Int, Int)
  -> ByteString
  -> sv a
  -> IO ()
{-# INLINEABLE generateSourcesAs #-}
generateSourcesAs = generateSourcesAtAs ucdLocate

generateTests :: (VG.Vector v a, Show a) => ByteString -> v a -> IO ()
{-# INLINEABLE generateTests #-}
generateTests = generateTestsAt ucdLocate

ucdLocate :: Locate
ucdLocate =
  Locate
    { path = ("unistring-ucd/generated/" <>)
    , moduleName = ("Data.Unistring.UCD.Internal." <>)
    , modulePath = ("Data/Unistring/UCD/Internal/" <>)
    , cSymbolName = ("ucd__" <>)
    }
