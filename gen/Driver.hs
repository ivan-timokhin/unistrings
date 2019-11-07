{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Driver where

import Control.Arrow ((&&&))
import Control.Concurrent.Async (concurrently_)
import Data.Bifunctor (first, second)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (GeneralCategory, toUpper)
import Data.Foldable (fold, for_)
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import Data.Word (Word32, Word8)
import System.IO (IOMode(WriteMode), hPrint, withFile)

import Data.UCD.Internal.Types
  ( Age
  , Block
  , DecompositionType
  , HangulSyllableType
  , Script
  )
import Gen
  ( ASCIISpec(ASCIISpec, asCPrefix)
  , EnumSpec(EnumSpec, esCPrefix, esHsType, esHsTypeModule)
  , IntSpec(IntSpec, isCPrefix, isHsType)
  , Module(moduleC, moduleHs)
  , generateASCII
  , generateEnum
  , generateIntegral
  , generateMayEnum
  , generateMonoContainer
  )
import Gen.Cost (SizedTy(sizeInBytes))
import Gen.Type
  ( IntegralType(itSize)
  , findTypeForRange
  , int64
  , typeASCII
  , typeContainer
  , typeContainerDedup
  , typeEnum
  , typeLayers
  , typeMEnum
  , word8
  )
import Trie (TrieDesc, mkTrie)
import TrieOpt (findOptimalPartitioning)

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

instance TableValue (Maybe Block) where
  type BottomType (Maybe Block) = IntegralType
  typeVals_ = typeMEnum
  generateModule prefix =
    generateMayEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "Block"
        , esHsTypeModule = "Data.UCD.Internal.Types"
        }

instance TableValue Script where
  type BottomType Script = IntegralType
  typeVals_ = typeEnum
  generateModule prefix =
    generateEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "Script"
        , esHsTypeModule = "Data.UCD.Internal.Types"
        }

instance TableValue (Maybe Age) where
  type BottomType (Maybe Age) = IntegralType
  typeVals_ = typeMEnum
  generateModule prefix =
    generateMayEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "Age"
        , esHsTypeModule = "Data.UCD.Internal.Types"
        }

instance TableValue (Maybe HangulSyllableType) where
  type BottomType (Maybe HangulSyllableType) = IntegralType
  typeVals_ = typeMEnum
  generateModule prefix =
    generateMayEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "HangulSyllableType"
        , esHsTypeModule = "Data.UCD.Internal.Types"
        }

instance TableValue (Maybe DecompositionType) where
  type BottomType (Maybe DecompositionType) = IntegralType
  typeVals_ = typeMEnum
  generateModule prefix =
    generateMayEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = "DecompositionType"
        , esHsTypeModule = "Data.UCD.Internal.Types"
        }

instance TableValue Word32 where
  type BottomType Word32 = IntegralType
  typeVals_ = typeEnum
  generateModule prefix =
    generateEnum
      EnumSpec
        {esCPrefix = prefix, esHsType = "Word32", esHsTypeModule = "Data.Word"}

instance TableValue (V.Vector Script) where
  type BottomType (V.Vector Script) = (IntegralType, V.Vector Word8)
  type BottomVal (V.Vector Script) = Int
  typeVals = first (second $ V.map $ toEnum . fromEnum) . typeContainerDedup
  generateModule = generateMonoContainer

instance TableValue Bool where
  type BottomType Bool = IntegralType
  typeVals_ = typeEnum
  generateModule prefix =
    generateEnum
      EnumSpec
        {esCPrefix = prefix, esHsType = "Bool", esHsTypeModule = "Data.Bool"}

instance TableValue (Maybe Bool) where
  type BottomType (Maybe Bool) = IntegralType
  typeVals_ = typeMEnum
  generateModule prefix =
    generateMayEnum
      EnumSpec
        {esCPrefix = prefix, esHsType = "Bool", esHsTypeModule = "Data.Bool"}

instance TableValue Word8 where
  type BottomType Word8 = IntegralType
  typeVals_ = const word8
  generateModule prefix =
    generateIntegral IntSpec {isCPrefix = prefix, isHsType = "Word8"}

instance TableValue Int64 where
  type BottomType Int64 = IntegralType
  typeVals_ = const int64
  generateModule prefix =
    generateIntegral IntSpec {isCPrefix = prefix, isHsType = "Int64"}

instance TableValue (Maybe Int) where
  type BottomType (Maybe Int) = IntegralType
  typeVals_ = typeMEnum
  generateModule prefix =
    generateMayEnum
      EnumSpec
        {esCPrefix = prefix, esHsType = "Int", esHsTypeModule = "Data.Int"}

instance TableValue Int where
  type BottomType Int = IntegralType
  typeVals_ = typeEnum
  generateModule prefix =
    generateEnum
      EnumSpec
        {esCPrefix = prefix, esHsType = "Int", esHsTypeModule = "Data.Int"}

instance TableValue ByteString where
  type BottomType ByteString = (IntegralType, ByteString)
  type BottomVal ByteString = Int
  typeVals = typeASCII
  generateModule prefix = generateASCII ASCIISpec {asCPrefix = prefix}

instance TableValue (V.Vector Word8) where
  type BottomType (V.Vector Word8) = (IntegralType, V.Vector Word8)
  type BottomVal (V.Vector Word8) = Int
  typeVals = typeContainer
  generateModule = generateMonoContainer

instance TableValue (V.Vector Int32) where
  type BottomType (V.Vector Int32) = (IntegralType, V.Vector Int32)
  type BottomVal (V.Vector Int32) = Int
  typeVals = typeContainer
  generateModule = generateMonoContainer

processTable ::
     forall a. TableValue a
  => (Int, Int)
  -> ByteString
  -> V.Vector a
  -> IO ()
{-# INLINE processTable #-}
processTable partitionings snakeName values = do
  generateSources partitionings snakeName values
  generateTests snakeName values

generateASCIITableSources ::
     (Int, Int) -> ByteString -> V.Vector ByteString -> IO ()
generateASCIITableSources partitionings snakeName values =
  concurrently_
    (generateSources partitionings (snakeName <> "_ptr") values)
    (generateSources partitionings (snakeName <> "_len") (fmap B.length values))

-- This assumes that the total length of all strings in a single
-- element is <= 255
generateASCIIVectorTableSources ::
     (Int, Int) -> ByteString -> V.Vector (V.Vector ByteString) -> IO ()
generateASCIIVectorTableSources partitionings snakeName values =
  concurrently_
    (generateSources partitionings (snakeName <> "_len") (fmap V.length values)) $
  concurrently_
    (generateSources
       partitionings
       (snakeName <> "_sublens")
       (fmap
          (\v ->
             if V.null v
               then V.empty
               else V.map (toEnum :: Int -> Word8) $
                    V.scanl' (\n str -> n + B.length str) 0 v)
          values))
    (generateSources partitionings (snakeName <> "_ptr") (fmap fold values))

generateSources ::
     forall a. TableValue a
  => (Int, Int)
  -> ByteString
  -> V.Vector a
  -> IO ()
{-# INLINEABLE generateSources #-}
generateSources (maxLayers, maxBits) snakeName values = do
  putStrLn $ show snakeName ++ " " ++ show p
  putStrLn $ show snakeName ++ " " ++ show cost
  let hsFile = "generated/hs/Data/UCD/Internal/" <> hsModuleName <> ".hs"
  B.writeFile (B.unpack hsFile) $
    B.unlines $
    "{-# OPTIONS_GHC -Wno-unused-imports #-}" :
    "{- HLINT ignore -}" :
    ("module Data.UCD.Internal." <> hsModuleName <> " (retrieve) where\n") :
    moduleHs modul
  B.writeFile (B.unpack $ "generated/cbits/" <> snakeName <> ".c") $
    B.unlines $ moduleC modul
  where
    hsModuleName = snake2camel snakeName
    modul = generateModule @a cprefix trie
    cprefix = "_hs__ucd__" <> snakeName
    trie = first (const bty) $ typeLayers $ mkTrie bvals p
    (cost, p) =
      findOptimalPartitioning
        (itSize . findTypeForRange)
        (sizeInBytes bty)
        maxBits
        maxLayers
        bvals
    -- trie = fromMaybe (error "Can't pick best trie") $ pickBest candidates
    -- candidates =
    --   map (first (const bty) . typeLayers) $ partitionings >>= mkTrieM bvals
    (bty, bvals) = typeVals values

generateTests :: Show a => ByteString -> V.Vector a -> IO ()
{-# INLINEABLE generateTests #-}
generateTests snakeName values =
  withFile (B.unpack $ "generated/test_data/" <> snakeName <> ".txt") WriteMode $ \h ->
    for_ values $ hPrint h

snake2camel :: ByteString -> ByteString
snake2camel = B.concat . map titlecase . B.split '_'
  where
    titlecase bstr =
      case B.uncons bstr of
        Nothing -> bstr
        Just (c, cs) -> B.cons (toUpper c) cs
