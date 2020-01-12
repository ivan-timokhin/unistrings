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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import Control.Exception (evaluate)
import Data.Word (Word16, Word32, Word8)
import GHC.Exts (IsList(fromList, toList))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (anyErrorCall, example, it, shouldThrow, testSpec)
import Test.Tasty.QuickCheck (Arbitrary, (===), testProperty)

#if defined(INSPECTION)
import qualified Inspection
#endif

import qualified Data.Unistring.Memory.Unsafe as U
import Data.Unistring.Singletons (Known)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Tests"
    [ testGroup
        "Memory"
        [ testGroup
            "Unsafe"
            [ testGroup "IsList" $
              let test ::
                       forall a storage alloc.
                       ( U.Primitive a
                       , Eq a
                       , Show a
                       , Arbitrary a
                       , Known storage
                       , U.Allocator storage alloc
                       )
                    => [TestTree]
                  test =
                    [ testProperty "toList . fromList == id" $ \(xs :: [a]) ->
                        let array :: U.Array alloc storage a
                            array = fromList xs
                         in toList array === xs
                    , testProperty "length . fromList == length" $ \(xs :: [a]) ->
                        let array :: U.Array alloc storage a
                            array = fromList xs
                         in U.getCountOf (U.arrayLength array) === length xs
                    ]
               in [ testGroup
                      "Native"
                      [ testGroup "Word8" (test @Word8 @'U.Native @U.Default)
                      , testGroup "Word16" (test @Word16 @'U.Native @U.Default)
                      , testGroup "Word32" (test @Word32 @'U.Native @U.Default)
                      ]
                  , testGroup
                      "Native pinned"
                      [ testGroup "Word8" (test @Word8 @'U.Native @U.Pinned)
                      , testGroup "Word16" (test @Word16 @'U.Native @U.Pinned)
                      , testGroup "Word32" (test @Word32 @'U.Native @U.Pinned)
                      ]
                  , testGroup
                      "Foreign"
                      [ testGroup "Word8" (test @Word8 @'U.Foreign @U.Pinned)
                      , testGroup "Word16" (test @Word16 @'U.Foreign @U.Pinned)
                      , testGroup "Word32" (test @Word32 @'U.Foreign @U.Pinned)
                      ]
                  ]
            , testGroup "Count validity checks" $
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
                      it
                        "doesn't throw when the element count is largest possible" $
                        example $
                        let maxCount :: U.CountOf a
                            maxCount = U.inElements maxBound
                         in () <$ evaluate (U.inBytes maxCount)
                      it
                        "throws when the element count is just a little bit too large" $
                        let maxCount :: U.CountOf a
                            maxCount = U.inElements maxBound
                         in evaluate (U.inBytes (maxCount + 1)) `shouldThrow`
                            anyErrorCall
               in [ test @Word8 "Word8"
                  , test @Word16 "Word16"
                  , test @Word32 "Word32"
                  ]
            ]
        ]
#if defined(INSPECTION)
    , Inspection.tests
#endif
    ]
