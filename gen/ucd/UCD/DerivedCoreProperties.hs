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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module UCD.DerivedCoreProperties where

import qualified Data.Map.Strict as Map

import UCD.Common (Table, fetchBinaryMulti)

type TB = Table () () Bool

data Props =
  Props
    { math :: TB
    , alphabetic :: TB
    , lowercase :: TB
    , uppercase :: TB
    , cased :: TB
    , caseIgnorable :: TB
    , changesWhenLowercased :: TB
    , changesWhenUppercased :: TB
    , changesWhenTitlecased :: TB
    , changesWhenCasefolded :: TB
    , changesWhenCasemapped :: TB
    , idStart :: TB
    , idContinue :: TB
    , xidStart :: TB
    , xidContinue :: TB
    , defaultIgnorableCodePoint :: TB
    , graphemeExtend :: TB
    , graphemeBase :: TB
    }

fetch :: IO Props
fetch = do
  m <- fetchBinaryMulti "DerivedCoreProperties.txt"
  let get prop =
        case Map.lookup prop m of
          Nothing -> fail $ "Can't find property " ++ show prop
          Just tbl -> pure tbl
  math <- get "Math"
  alphabetic <- get "Alphabetic"
  lowercase <- get "Lowercase"
  uppercase <- get "Uppercase"
  cased <- get "Cased"
  caseIgnorable <- get "Case_Ignorable"
  changesWhenLowercased <- get "Changes_When_Lowercased"
  changesWhenUppercased <- get "Changes_When_Uppercased"
  changesWhenTitlecased <- get "Changes_When_Titlecased"
  changesWhenCasefolded <- get "Changes_When_Casefolded"
  changesWhenCasemapped <- get "Changes_When_Casemapped"
  idStart <- get "ID_Start"
  idContinue <- get "ID_Continue"
  xidStart <- get "XID_Start"
  xidContinue <- get "XID_Continue"
  defaultIgnorableCodePoint <- get "Default_Ignorable_Code_Point"
  graphemeExtend <- get "Grapheme_Extend"
  graphemeBase <- get "Grapheme_Base"
  pure Props {..}
