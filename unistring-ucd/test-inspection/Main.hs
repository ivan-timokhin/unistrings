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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Main
  ( main
  ) where

import Data.Maybe (isJust)
import Test.Inspection (hasNoType, inspect)

import qualified Data.Unistring.UCD as UCD

--------------------------------------------------------------------------------
-- List fusion
--------------------------------------------------------------------------------
aliasesLength :: UCD.CodePoint -> Int
aliasesLength cp = length (UCD.nameAliases cp)

inspect $ 'aliasesLength `hasNoType` ''[]

scriptExtensionsLength :: UCD.CodePoint -> Int
scriptExtensionsLength cp = length (UCD.scriptExtensionsRaw cp)

inspect $ 'scriptExtensionsLength `hasNoType` ''[]

nontrivialCanonicalDecompositionLength :: UCD.CodePoint -> Int
nontrivialCanonicalDecompositionLength cp =
  length (UCD.nontrivialCanonicalDecomposition cp)

inspect $ 'nontrivialCanonicalDecompositionLength `hasNoType` ''[]

nontrivialCompatibilityDecompositionLength :: UCD.CodePoint -> Int
nontrivialCompatibilityDecompositionLength cp =
  length (UCD.nontrivialCompatibilityDecomposition cp)

inspect $ 'nontrivialCompatibilityDecompositionLength `hasNoType` ''[]

nfkcCaseFoldLength :: UCD.CodePoint -> Int
nfkcCaseFoldLength cp =
  case UCD.nfkcCaseFold cp of
    UCD.ShortCF _ -> 1
    UCD.LongCF cs -> length cs

inspect $ 'nfkcCaseFoldLength `hasNoType` ''[]

--------------------------------------------------------------------------------
-- Maybe elimination
--------------------------------------------------------------------------------
blockIx :: UCD.CodePoint -> Int
blockIx cp = maybe 0 fromEnum (UCD.block cp)

inspect $ 'blockIx `hasNoType` ''Maybe

ageIx :: UCD.CodePoint -> Int
ageIx cp = maybe 0 fromEnum (UCD.age cp)

inspect $ 'ageIx `hasNoType` ''Maybe

hstIx :: UCD.CodePoint -> Int
hstIx cp = maybe 0 fromEnum (UCD.hangulSyllableType cp)

inspect $ 'hstIx `hasNoType` ''Maybe

isNumeric :: UCD.CodePoint -> Bool
isNumeric cp = isJust (UCD.numeric cp)

inspect $ 'isNumeric `hasNoType` ''Maybe

hasDecomposition :: UCD.CodePoint -> Bool
hasDecomposition cp = isJust (UCD.decompositionType cp)

inspect $ 'hasDecomposition `hasNoType` ''Maybe

areComposable :: UCD.CodePoint -> UCD.CodePoint -> Bool
areComposable c1 c2 = isJust (UCD.canonicalComposition c1 c2)

inspect $ 'areComposable `hasNoType` ''Maybe

nfcQC :: UCD.CodePoint -> Bool
nfcQC c = isJust (UCD.nfcQuickCheck c)

inspect $ 'nfcQC `hasNoType` ''Maybe

nfkcQC :: UCD.CodePoint -> Bool
nfkcQC c = isJust (UCD.nfkcQuickCheck c)

inspect $ 'nfkcQC `hasNoType` ''Maybe

joiningGroup :: UCD.CodePoint -> Bool
joiningGroup c = isJust (UCD.joiningGroup c)

inspect $ 'joiningGroup `hasNoType` ''Maybe

bidiMirroringGlyph :: UCD.CodePoint -> Bool
bidiMirroringGlyph c = isJust (UCD.bidiMirroringGlyph c)

inspect $ 'bidiMirroringGlyph `hasNoType` ''Maybe

bidiPairedBracketType :: UCD.CodePoint -> Bool
bidiPairedBracketType c = isJust (UCD.bidiPairedBracketType c)

inspect $ 'bidiPairedBracketType `hasNoType` ''Maybe

equivalentUnifiedIdeograph :: UCD.CodePoint -> Bool
equivalentUnifiedIdeograph c = isJust (UCD.equivalentUnifiedIdeograph c)

inspect $ 'equivalentUnifiedIdeograph `hasNoType` ''Maybe

indicPositionalCategory :: UCD.CodePoint -> Bool
indicPositionalCategory c = isJust (UCD.indicPositionalCategory c)

inspect $ 'indicPositionalCategory `hasNoType` ''Maybe

main :: IO ()
main = pure ()
