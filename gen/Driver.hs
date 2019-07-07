{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Driver where

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (GeneralCategory, toUpper)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Word (Word8)
import System.IO (IOMode(WriteMode), hPrint, withFile)

import Gen
  ( EnumSpec(EnumSpec, esCPrefix, esHsType, esHsTypeModule)
  , IntSpec(IntSpec, isCPrefix, isHsType)
  , Module(moduleC, moduleHs)
  , generateEnum
  , generateIntegral
  )
import Gen.Cost (SizedTy, pickBest, totalCost)
import Gen.Type (IntegralType, typeEnum, typeLayers, word8)
import ListM (generatePartitionings)
import Trie (TrieDesc, mkTrieM, partitioning)

class (SizedTy (BottomType a), Ord (BottomVal a), Show a) =>
      TableValue a
  where
  type BottomType a
  type BottomVal a
  type BottomVal a = a
  {-# MINIMAL (typeVals | typeVals_), generateModule #-}
  typeVals :: V.Vector a -> (BottomType a, V.Vector (BottomVal a))
  default typeVals :: (BottomVal a ~ a) =>
    V.Vector a -> (BottomType a, V.Vector (BottomVal a))
  typeVals = typeVals_ &&& id
  typeVals_ :: V.Vector a -> BottomType a
  typeVals_ = fst . typeVals
  generateModule ::
       ByteString
    -> TrieDesc Identity IntegralType (BottomType a) (BottomVal a)
    -> Module

instance TableValue GeneralCategory where
  type BottomType GeneralCategory = IntegralType
  typeVals_ = typeEnum
  generateModule prefix =
    generateEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "GeneralCategory"
        , esHsTypeModule = "Data.Char"
        }

instance TableValue Word8 where
  type BottomType Word8 = IntegralType
  typeVals_ = const word8
  generateModule prefix =
    generateIntegral IntSpec {isCPrefix = prefix, isHsType = "Word8"}

processTable ::
     forall a. TableValue a
  => ByteString
  -> V.Vector a
  -> IO ()
{-# INLINE processTable #-}
processTable snakeName values = do
  print $ partitioning trie
  print $ totalCost trie
  let hsFile = "generated/hs/Data/UCD/Internal/" <> hsModuleName <> ".hs"
  B.writeFile (B.unpack hsFile) $
    B.unlines $
    "{-# OPTIONS_GHC -Wno-unused-imports #-}" :
    ("module Data.UCD.Internal." <> hsModuleName <> " (retrieve) where\n") :
    moduleHs modul
  B.writeFile (B.unpack $ "generated/cbits/" <> snakeName <> ".c") $
    B.unlines $ moduleC modul
  withFile (B.unpack $ "generated/test_data/" <> snakeName <> ".txt") WriteMode $ \h ->
    for_ values $ hPrint h
  where
    hsModuleName = snake2camel snakeName
    modul = generateModule @a cprefix trie
    cprefix = "_hs__ucd__" <> snakeName
    trie = fromMaybe (error "Can't pick best trie") $ pickBest candidates
    candidates =
      map (first (const bty) . typeLayers) $ partitionings >>= mkTrieM bvals
    partitionings = generatePartitionings 4 0 16
    (bty, bvals) = typeVals values

snake2camel :: ByteString -> ByteString
snake2camel = B.concat . map titlecase . B.split '_'
  where
    titlecase bstr =
      case B.uncons bstr of
        Nothing -> bstr
        Just (c, cs) -> B.cons (toUpper c) cs
