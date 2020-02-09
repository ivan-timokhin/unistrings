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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Inspection.Unistring.Scalar.Value (tests) where

import Test.Tasty (TestTree, testGroup)
import Data.Maybe (fromMaybe)

import qualified Data.Unistring.UCD as UCD

import qualified Data.Unistring.Scalar.Value as SV

import Inspection.TH (allHaveNoneOfTypes, inspectTests, hasNoneOfTypes)

tests :: [TestTree]
tests =
  [ testGroup
      "Decomposition fusion"
      $(inspectTests $
        ['foldNontrivialCD, 'foldNontrivialKD] `allHaveNoneOfTypes` [''[]])
  , testGroup
      "Maybe fusion"
      $(inspectTests $
        [ 'withDefaultBMG
        , 'withDefaultEUI
        , 'withDefaultComposition
        , 'withDefaultComposition'
        ] `allHaveNoneOfTypes`
        [''Maybe])
  , testGroup
      "Case mappings"
      $(inspectTests $
        ['foldLowercase, 'foldUppercase, 'foldTitlecase, 'foldCaseFolding] `allHaveNoneOfTypes`
        [''SV.CaseMapping, ''UCD.CaseMapping])
  , testGroup
      "NFKC_Casefold fusion"
      $(inspectTests $
        'foldNfkcCf `hasNoneOfTypes`
        [''[], ''SV.NFKCCaseFold, ''UCD.NFKCCaseFold])
  ]

foldNontrivialCD :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldNontrivialCD f z = foldr f z . SV.nontrivialCanonicalDecomposition

foldNontrivialKD :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldNontrivialKD f z = foldr f z . SV.nontrivialCompatibilityDecomposition

withDefaultBMG :: SV.ScalarValue -> SV.ScalarValue -> SV.ScalarValue
withDefaultBMG def sv = fromMaybe def $ SV.bidiMirroringGlyph sv

withDefaultEUI :: SV.ScalarValue -> SV.ScalarValue -> SV.ScalarValue
withDefaultEUI def sv = fromMaybe def $ SV.equivalentUnifiedIdeograph sv

withDefaultComposition ::
     SV.ScalarValue -> SV.ScalarValue -> SV.ScalarValue -> SV.ScalarValue
withDefaultComposition def s1 s2 = fromMaybe def $ SV.canonicalComposition s1 s2

withDefaultComposition' ::
     SV.ScalarValue -> SV.ScalarValue -> SV.ScalarValue -> SV.ScalarValue
withDefaultComposition' def s1 s2 =
  fromMaybe def $
  SV.canonicalCompositionStart s1 >>= flip SV.canonicalCompositionFinish s2

foldCM :: (SV.ScalarValue -> r -> r) -> r -> SV.CaseMapping -> r
{-# INLINE foldCM #-}
foldCM f z cm =
  case cm of
    SV.SingleCM c -> f c z
    SV.DoubleCM c1 c2 -> f c1 $ f c2 z
    SV.TripleCM c1 c2 c3 -> f c1 $ f c2 $ f c3 z

foldLowercase :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldLowercase f z = foldCM f z . SV.lowercaseMapping

foldUppercase :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldUppercase f z = foldCM f z . SV.uppercaseMapping

foldTitlecase :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldTitlecase f z = foldCM f z . SV.titlecaseMapping

foldCaseFolding :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldCaseFolding f z = foldCM f z . SV.caseFolding

foldNfkcCf :: (SV.ScalarValue -> r -> r) -> r -> SV.ScalarValue -> r
foldNfkcCf f z sv = case SV.nfkcCaseFold sv of
  SV.ShortCF s -> f s z
  SV.LongCF ss -> foldr f z ss
