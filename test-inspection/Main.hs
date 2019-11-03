{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import Data.Maybe (isJust)
import Test.Inspection (hasNoType, inspect)

import qualified Data.UCD as UCD

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

main :: IO ()
main = pure ()
