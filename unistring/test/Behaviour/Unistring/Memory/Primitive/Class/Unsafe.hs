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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Behaviour.Unistring.Memory.Primitive.Class.Unsafe
  ( tests
  ) where

import Control.Exception (evaluate)
import Data.Word (Word16, Word32, Word8)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (anyErrorCall, example, it, shouldThrow, testSpec)

import qualified Data.Unistring.Memory.Count as U
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as U

tests :: [TestTree]
tests =
  [ testGroup "Count validity checks" $
    let test ::
             forall a. U.Primitive a
          => String
          -> TestTree
        test name =
          unsafePerformIO $
          testSpec name $ do
            it "throws when element count is negative" $
              evaluate (U.inBytes ((-1) :: U.CountOf a)) `shouldThrow`
              anyErrorCall
            it "throws when element count is negative and large" $
              evaluate (U.inBytes (minBound :: U.CountOf a)) `shouldThrow`
              anyErrorCall
            it "doesn't throw when the element count is largest possible" $
              example $
              let maxCount :: U.CountOf a
                  maxCount = U.inElements maxBound
               in () <$ evaluate (U.inBytes maxCount)
            it "throws when the element count is just a little bit too large" $
              let maxCount :: U.CountOf a
                  maxCount = U.inElements maxBound
               in evaluate (U.inBytes (maxCount + 1)) `shouldThrow` anyErrorCall
     in [test @Word8 "Word8", test @Word16 "Word16", test @Word32 "Word32"]
  ]
