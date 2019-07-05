{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Driver where

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
import Gen.Type (EnumTy, IntTy, typeEnum, typeIntegral, word8)
import ListM (generatePartitionings)
import Trie (BottomAnnotation, LayerAnnotation, TrieDesc, mkTrieM, partitioning)

class ( SizedTy (BottomAnnotation (TrieTy a))
      , SizedTy (LayerAnnotation (TrieTy a))
      , Ord a
      , Show a
      ) =>
      TableValue a
  where
  type TrieTy a
  typeTrie :: Traversable f => TrieDesc ann f a -> TrieDesc (TrieTy a) f a
  generateModule :: ByteString -> TrieDesc (TrieTy a) Identity a -> Module

instance TableValue GeneralCategory where
  type TrieTy GeneralCategory = EnumTy
  typeTrie = typeEnum
  generateModule prefix =
    generateEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "GeneralCategory"
        , esHsTypeModule = "Data.Char"
        }

instance TableValue Word8 where
  type TrieTy Word8 = IntTy
  typeTrie = typeIntegral word8
  generateModule prefix =
    generateIntegral IntSpec {isCPrefix = prefix, isHsType = "Word8"}

processTable :: TableValue a => ByteString -> V.Vector a -> IO ()
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
    hsModuleName = snake2camel $ snakeName
    modul = generateModule cprefix trie
    cprefix = "_hs__ucd__" <> snakeName
    trie = fromMaybe (error "Can't pick best trie") $ pickBest candidates
    candidates = map typeTrie $ partitionings >>= mkTrieM values
    partitionings = generatePartitionings 4 0 16

snake2camel :: ByteString -> ByteString
snake2camel = B.concat . map titlecase . B.split '_'
  where
    titlecase bstr =
      case B.uncons bstr of
        Nothing -> bstr
        Just (c, cs) -> B.cons (toUpper c) cs
