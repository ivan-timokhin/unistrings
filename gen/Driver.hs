{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Driver where

import Control.Arrow ((&&&))
import Control.Concurrent.Async (concurrently_)
import Data.Bifunctor (first, second)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (GeneralCategory, toUpper)
import Data.Coerce (coerce)
import Data.Foldable (fold, for_)
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.Typeable
  ( Proxy(Proxy)
  , Typeable
  , tyConModule
  , tyConName
  , typeRep
  , typeRepTyCon
  )
import qualified Data.Vector as V
import Data.Word (Word32, Word8)
import System.IO (IOMode(WriteMode), hPrint, withFile)

import Data.UCD.Internal.Types
  ( Age
  , Block
  , DecompositionType
  , GraphemeClusterBreak
  , HangulSyllableType
  , JoiningGroup
  , JoiningType
  , LineBreak
  , Script
  , SentenceBreak
  , VerticalOrientation
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
  , generateMayIntegral
  , generateMonoContainer
  )
import Gen.Cost (SizedTy(sizeInBytes))
import Gen.Type
  ( IntegralType(itSize)
  , findTypeForRange
  , typeASCII
  , typeContainer
  , typeContainerDedup
  , typeEnum
  , typeIntegral
  , typeLayers
  , typeMEnum
  , typeMIntegral
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

typeName ::
     forall a. Typeable a
  => ByteString
typeName = B.pack $ tyConName $ typeRepTyCon $ typeRep $ Proxy @a

moduleName ::
     forall a. Typeable a
  => ByteString
moduleName = B.pack $ tyConModule $ typeRepTyCon $ typeRep $ Proxy @a

newtype TableEnum e =
  TableEnum e
  deriving (Show, Eq, Ord)

instance (Enum e, Ord e, Show e, Typeable e) => TableValue (TableEnum e) where
  type BottomType (TableEnum e) = IntegralType
  type BottomVal (TableEnum e) = e
  typeVals vals = (typeEnum evals, evals)
    where
      evals = coerce vals
  generateModule prefix =
    generateEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = typeName @e
        , esHsTypeModule = moduleName @e
        }

deriving via TableEnum Script instance TableValue Script

deriving via TableEnum JoiningType instance TableValue JoiningType

deriving via TableEnum VerticalOrientation instance
         TableValue VerticalOrientation

deriving via TableEnum LineBreak instance TableValue LineBreak

deriving via TableEnum GraphemeClusterBreak instance
         TableValue GraphemeClusterBreak

deriving via TableEnum SentenceBreak instance
         TableValue SentenceBreak

deriving via TableEnum GeneralCategory instance
         TableValue GeneralCategory

newtype TableMayEnum e =
  TableMayEnum
    { getTableMayEnum :: Maybe e
    }
  deriving (Show, Eq, Ord)

instance (Enum e, Show e, Ord e, Typeable e) =>
         TableValue (TableMayEnum e) where
  type BottomType (TableMayEnum e) = IntegralType
  type BottomVal (TableMayEnum e) = Maybe e
  typeVals vals = (typeMEnum evals, evals)
    where
      evals = coerce vals
  generateModule prefix =
    generateMayEnum
      EnumSpec
        { esCPrefix = prefix
        , esHsType = typeName @e
        , esHsTypeModule = moduleName @e
        }

deriving via TableMayEnum Block instance TableValue (Maybe Block)

deriving via TableMayEnum Age instance TableValue (Maybe Age)

deriving via TableMayEnum HangulSyllableType instance
         TableValue (Maybe HangulSyllableType)

deriving via TableMayEnum DecompositionType instance
         TableValue (Maybe DecompositionType)

deriving via TableMayEnum JoiningGroup instance
         TableValue (Maybe JoiningGroup)

newtype TableIntegral i =
  TableIntegral
    { getTableIntegral :: i
    }
  deriving (Show, Eq, Ord)

instance (Integral i, Show i, Ord i, Typeable i) =>
         TableValue (TableIntegral i) where
  type BottomType (TableIntegral i) = IntegralType
  type BottomVal (TableIntegral i) = i
  typeVals vals = (typeIntegral ivals, ivals)
    where
      ivals = coerce vals
  generateModule prefix =
    generateIntegral IntSpec {isCPrefix = prefix, isHsType = typeName @i}

deriving via TableIntegral Word32 instance TableValue Word32

deriving via TableIntegral Word8 instance TableValue Word8

deriving via TableIntegral Int64 instance TableValue Int64

deriving via TableIntegral Int instance TableValue Int

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

instance TableValue (V.Vector Script) where
  type BottomType (V.Vector Script) = (IntegralType, V.Vector Word8)
  type BottomVal (V.Vector Script) = Int
  typeVals = first (second $ V.map $ toEnum . fromEnum) . typeContainerDedup
  generateModule = generateMonoContainer

instance TableValue (Maybe Int) where
  type BottomType (Maybe Int) = IntegralType
  typeVals_ = typeMIntegral
  generateModule prefix =
    generateMayIntegral IntSpec {isCPrefix = prefix, isHsType = "Int"}

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
    "{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}" :
    "{- HLINT ignore -}" :
    "{-# LANGUAGE MagicHash #-}" :
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
