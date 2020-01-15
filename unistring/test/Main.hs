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

module Main
  ( main
  ) where

import Control.Monad (when)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, testGroup)
import Test.Tasty.Runners.AntXML (antXMLRunner)

#if defined(INSPECTION)
import qualified Inspection
#endif

import qualified Behaviour

main :: IO ()
main = do
  xmlFile <- lookupEnv "TASTY_XML"
  when (xmlFile == Just "CI") $ setEnv "TASTY_XML" "TEST-unistring.xml"
  defaultMainWithIngredients (antXMLRunner : defaultIngredients) $
    testGroup
      "Tests"
      [ testGroup "Behaviour" Behaviour.tests
#if defined(INSPECTION)
      , testGroup "Inspection" Inspection.tests
#endif
      ]
