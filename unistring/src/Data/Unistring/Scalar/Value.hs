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
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Scalar.Value
Description : Unicode scalar value type
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Scalar.Value
  ( ScalarValue
  , checkScalarValue
  -- * UCD properties
  -- ** General
  , UCD.name
  , UCD.nameAliases
  , UCD.NameAliasType(..)
  , UCD.block
  , UCD.Block(..)
  , UCD.age
  , UCD.Age(..)
  , UCD.generalCategory
  , UCD.GeneralCategory(..)
  , UCD.script
  , UCD.Script(..)
  , UCD.scriptExtensions
  , UCD.scriptExtensionsRaw
  , UCD.whiteSpace
  , UCD.alphabetic
  , UCD.hangulSyllableType
  , UCD.HangulSyllableType(..)
  , UCD.noncharacterCodePoint
  , UCD.defaultIgnorableCodePoint
  , UCD.deprecated
  , UCD.logicalOrderException
  , UCD.variationSelector
  -- ** Case
  , UCD.uppercase
  , UCD.lowercase
  , lowercaseMapping
  , uppercaseMapping
  , titlecaseMapping
  , caseFolding
  , CaseMapping(..)
  , simpleLowercaseMapping
  , simpleUppercaseMapping
  , simpleTitlecaseMapping
  , simpleCaseFolding
  , UCD.softDotted
  , UCD.cased
  , UCD.caseIgnorable
  , UCD.changesWhenLowercased
  , UCD.changesWhenUppercased
  , UCD.changesWhenTitlecased
  , UCD.changesWhenCasefolded
  , UCD.changesWhenCasemapped
  -- ** Numeric
  , UCD.numeric
  , UCD.Numeric(..)
  , UCD.hexDigit
  , UCD.asciiHexDigit
  -- ** Normalisation
  , UCD.canonicalCombiningClass
  , canonicalDecomposition
  , compatibilityDecomposition
  , nontrivialCanonicalDecomposition
  , nontrivialCompatibilityDecomposition
  , canonicalComposition
  , canonicalCompositionStart
  , canonicalCompositionFinish
  , CompositionToken
  , UCD.decompositionType
  , UCD.DecompositionType(..)
  , UCD.nfdQuickCheck
  , UCD.nfcQuickCheck
  , UCD.nfkdQuickCheck
  , UCD.nfkcQuickCheck
  , nfkcCaseFold
  , NFKCCaseFold(ShortCF, LongCF)
  , UCD.changesWhenNFKCCasefolded
  -- ** Shaping and Rendering
  , UCD.joinControl
  , UCD.joiningGroup
  , UCD.JoiningGroup(..)
  , UCD.joiningType
  , UCD.JoiningType(..)
  , UCD.verticalOrientation
  , UCD.VerticalOrientation(..)
  , UCD.lineBreak
  , UCD.LineBreak(..)
  , UCD.graphemeClusterBreak
  , UCD.GraphemeClusterBreak(..)
  , UCD.sentenceBreak
  , UCD.SentenceBreak(..)
  , UCD.wordBreak
  , UCD.WordBreak(..)
  , UCD.eastAsianWidth
  , UCD.EastAsianWidth(..)
  , UCD.prependedConcatenationMark
  -- ** Bidirectional
  , UCD.bidiClass
  , UCD.BidiClass(..)
  , UCD.bidiControl
  , UCD.bidiMirrored
  , bidiMirroringGlyph
  , bidiPairedBracket
  , UCD.bidiPairedBracketType
  , UCD.BidiPairedBracketType(..)
  -- ** Identifiers
  , UCD.idContinue
  , UCD.idStart
  , UCD.xidContinue
  , UCD.xidStart
  , UCD.patternSyntax
  , UCD.patternWhiteSpace
  -- ** CJK
  , UCD.ideographic
  , UCD.unifiedIdeograph
  , UCD.radical
  , UCD.idsBinaryOperator
  , UCD.idsTrinaryOperator
  , equivalentUnifiedIdeograph
  -- ** Miscellaneous
  , UCD.math
  , UCD.quotationMark
  , UCD.dash
  , UCD.sentenceTerminal
  , UCD.terminalPunctuation
  , UCD.diacritic
  , UCD.extender
  , UCD.graphemeBase
  , UCD.graphemeExtend
  , UCD.unicode1Name
  , UCD.regionalIndicator
  , UCD.indicPositionalCategory
  , UCD.IndicPositionalCategory(..)
  , UCD.indicSyllabicCategory
  , UCD.IndicSyllabicCategory(..)
  ) where

import qualified Data.Unistring.UCD as UCD

import Data.Unistring.Scalar.Value.Unsafe
  ( ScalarValue(ScalarValue)
  , checkScalarValue
  )

simpleLowercaseMapping :: ScalarValue -> ScalarValue
simpleLowercaseMapping = ScalarValue . UCD.simpleLowercaseMapping

simpleUppercaseMapping :: ScalarValue -> ScalarValue
simpleUppercaseMapping = ScalarValue . UCD.simpleUppercaseMapping

simpleTitlecaseMapping :: ScalarValue -> ScalarValue
simpleTitlecaseMapping = ScalarValue . UCD.simpleTitlecaseMapping

simpleCaseFolding :: ScalarValue -> ScalarValue
simpleCaseFolding = ScalarValue . UCD.simpleCaseFolding

nontrivialCanonicalDecomposition :: ScalarValue -> [ScalarValue]
{-# INLINE nontrivialCanonicalDecomposition #-}
nontrivialCanonicalDecomposition sv =
  map ScalarValue $ UCD.nontrivialCanonicalDecomposition sv

canonicalDecomposition :: ScalarValue -> [ScalarValue]
{-# INLINE canonicalDecomposition #-}
canonicalDecomposition sv = map ScalarValue $ UCD.canonicalDecomposition sv

nontrivialCompatibilityDecomposition :: ScalarValue -> [ScalarValue]
{-# INLINE nontrivialCompatibilityDecomposition #-}
nontrivialCompatibilityDecomposition sv =
  map ScalarValue $ UCD.nontrivialCompatibilityDecomposition sv

compatibilityDecomposition :: ScalarValue -> [ScalarValue]
{-# INLINE compatibilityDecomposition #-}
compatibilityDecomposition sv =
  map ScalarValue $ UCD.compatibilityDecomposition sv

bidiMirroringGlyph :: ScalarValue -> Maybe ScalarValue
bidiMirroringGlyph sv = ScalarValue <$> UCD.bidiMirroringGlyph sv

bidiPairedBracket :: ScalarValue -> ScalarValue
bidiPairedBracket = ScalarValue . UCD.bidiPairedBracket

equivalentUnifiedIdeograph :: ScalarValue -> Maybe ScalarValue
equivalentUnifiedIdeograph sv =
  ScalarValue <$> UCD.equivalentUnifiedIdeograph sv

data CaseMapping
  = SingleCM {-# UNPACK #-}!ScalarValue
  | DoubleCM {-# UNPACK #-}!ScalarValue {-# UNPACK #-}!ScalarValue
  | TripleCM
      {-# UNPACK #-}!ScalarValue
      {-# UNPACK #-}!ScalarValue
      {-# UNPACK #-}!ScalarValue
  deriving (Show, Eq)

ucdToSvCM :: UCD.CaseMapping -> CaseMapping
{-# INLINE ucdToSvCM #-}
ucdToSvCM (UCD.SingleCM cp) = SingleCM (ScalarValue cp)
ucdToSvCM (UCD.DoubleCM c1 c2) = DoubleCM (ScalarValue c1) (ScalarValue c2)
ucdToSvCM (UCD.TripleCM c1 c2 c3) =
  TripleCM (ScalarValue c1) (ScalarValue c2) (ScalarValue c3)

lowercaseMapping :: ScalarValue -> CaseMapping
lowercaseMapping = ucdToSvCM . UCD.lowercaseMapping

uppercaseMapping :: ScalarValue -> CaseMapping
uppercaseMapping = ucdToSvCM . UCD.uppercaseMapping

titlecaseMapping :: ScalarValue -> CaseMapping
titlecaseMapping = ucdToSvCM . UCD.titlecaseMapping

caseFolding :: ScalarValue -> CaseMapping
caseFolding = ucdToSvCM . UCD.caseFolding

data NFKCCaseFold
  = ShortCF {-# UNPACK #-}!ScalarValue
  | LongCF [ScalarValue]
  deriving (Show, Eq)

ucdToSvNFKCCF :: UCD.NFKCCaseFold -> NFKCCaseFold
{-# INLINE ucdToSvNFKCCF #-}
ucdToSvNFKCCF (UCD.ShortCF cp) = ShortCF (ScalarValue cp)
ucdToSvNFKCCF (UCD.LongCF cps) = LongCF (map ScalarValue cps)

nfkcCaseFold :: ScalarValue -> NFKCCaseFold
{-# INLINE nfkcCaseFold #-}
nfkcCaseFold = ucdToSvNFKCCF . UCD.nfkcCaseFold

newtype CompositionToken =
  CT UCD.CompositionToken

canonicalCompositionStart :: ScalarValue -> Maybe CompositionToken
{-# INLINE canonicalCompositionStart #-}
canonicalCompositionStart sv = CT <$> UCD.canonicalCompositionStart sv

canonicalCompositionFinish ::
     CompositionToken -> ScalarValue -> Maybe ScalarValue
{-# INLINE canonicalCompositionFinish #-}
canonicalCompositionFinish (CT ct) sv =
  ScalarValue <$> UCD.canonicalCompositionFinish ct sv

canonicalComposition :: ScalarValue -> ScalarValue -> Maybe ScalarValue
{-# INLINE canonicalComposition #-}
canonicalComposition sv1 sv2 = ScalarValue <$> UCD.canonicalComposition sv1 sv2
