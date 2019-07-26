{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import Test.Inspection

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

main :: IO ()
main = pure ()
