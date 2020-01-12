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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Inspection.TH
  ( inspectTest
  ) where

import Language.Haskell.TH.Syntax (Exp, Q)
import qualified Test.Inspection as I
import qualified Test.Tasty.Providers as T

inspectionResultToTastyResult :: I.Result -> T.Result
inspectionResultToTastyResult =
  \case
    I.Failure msg -> T.testFailed msg
    I.Success msg -> T.testPassed msg

newtype PrerunTest =
  PrerunTest T.Result

instance T.IsTest PrerunTest where
  run _ (PrerunTest result) _ = pure result
  testOptions = mempty

inspectTest :: T.TestName -> I.Obligation -> Q Exp
inspectTest name obl =
  [|T.singleTest name $
    PrerunTest $ inspectionResultToTastyResult $(I.inspectTest obl)|]
