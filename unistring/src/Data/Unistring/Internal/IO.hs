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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Unistring.Internal.IO
  ( readWritePerformIO
  , readOnlyPerformIO
  ) where

import GHC.Base (realWorld#)
import GHC.Magic (runRW#)
import GHC.IO (IO(IO))

-- See note ‘Perform IO’
readWritePerformIO :: IO a -> a
{-# INLINE readWritePerformIO #-}
readWritePerformIO (IO f) =
  case runRW# f of
    (# _, a #) -> a

-- See note ‘Perform IO’
readOnlyPerformIO :: IO a -> a
{-# INLINE readOnlyPerformIO #-}
readOnlyPerformIO (IO f) =
  case f realWorld# of
    (# _, a #) -> a
