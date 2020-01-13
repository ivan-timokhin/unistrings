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
module Behaviour
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)

import qualified Behaviour.Unistring.Memory.Unsafe

tests :: [TestTree]
tests =
  [ testGroup
      "Unistring"
      [ testGroup
          "Memory"
          [testGroup "Unsafe" Behaviour.Unistring.Memory.Unsafe.tests]
      ]
  ]
