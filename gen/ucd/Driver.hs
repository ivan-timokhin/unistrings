{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Driver where

import Control.Arrow ((&&&))
import Data.Bifunctor (first, second)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Typeable
  ( Proxy(Proxy)
  , Typeable
  , tyConModule
  , tyConName
  , typeRep
  , typeRepTyCon
  )
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)
import System.IO (IOMode(WriteMode), hPrint, withFile)

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
  ( FFIIntegralType
  , IntegralType(itSize)
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

import qualified Runner as R

typeName ::
     forall a. Typeable a
  => ByteString
typeName = B.pack $ tyConName $ typeRepTyCon $ typeRep $ Proxy @a

moduleName ::
     forall a. Typeable a
  => ByteString
moduleName = B.pack $ tyConModule $ typeRepTyCon $ typeRep $ Proxy @a

data TableValue sv dv a bv bt =
  TableValue
    { typeValues :: sv a -> (bt, dv bv)
    , generateModule :: ByteString -> TrieDesc Identity IntegralType bt bv -> Module
    }

enum ::
     forall e. (Enum e, Typeable e)
  => TableValue V.Vector V.Vector e e IntegralType
enum =
  TableValue
    { typeValues = typeEnum &&& id
    , generateModule =
        \prefix ->
          generateEnum
            EnumSpec
              { esCPrefix = prefix
              , esHsType = typeName @e
              , esHsTypeModule = moduleName @e
              }
    }

maybeEnum ::
     forall e. (Enum e, Typeable e)
  => TableValue V.Vector V.Vector (Maybe e) (Maybe e) IntegralType
maybeEnum =
  TableValue
    { typeValues = typeMEnum &&& id
    , generateModule =
        \prefix ->
          generateMayEnum
            EnumSpec
              { esCPrefix = prefix
              , esHsType = typeName @e
              , esHsTypeModule = moduleName @e
              }
    }

integral ::
     forall i. (Integral i, Typeable i, VU.Unbox i)
  => TableValue VU.Vector VU.Vector i i IntegralType
integral =
  TableValue
    { typeValues = typeIntegral &&& id
    , generateModule =
        \prefix ->
          generateIntegral IntSpec {isCPrefix = prefix, isHsType = typeName @i}
    }

maybeIntegral ::
     forall i. (Integral i, Typeable i)
  => TableValue V.Vector V.Vector (Maybe i) (Maybe i) IntegralType
maybeIntegral =
  TableValue
    { typeValues = typeMIntegral &&& id
    , generateModule =
        \prefix ->
          generateMayIntegral IntSpec {isCPrefix = prefix, isHsType = "Int"}
    }

bool :: TableValue V.Vector V.Vector Bool Bool IntegralType
bool =
  TableValue
    { typeValues = typeEnum &&& id
    , generateModule =
        \prefix ->
          generateEnum
            EnumSpec
              { esCPrefix = prefix
              , esHsType = "Bool"
              , esHsTypeModule = "Data.Bool"
              }
    }

maybeBool :: TableValue V.Vector V.Vector (Maybe Bool) (Maybe Bool) IntegralType
maybeBool =
  TableValue
    { typeValues = typeMEnum &&& id
    , generateModule =
        \prefix ->
          generateMayEnum
            EnumSpec
              { esCPrefix = prefix
              , esHsType = "Bool"
              , esHsTypeModule = "Data.Bool"
              }
    }

-- Only for enumerations with < 256 elements; resulting table will
-- contain Word8
smallEnumVector ::
     (Enum e, Ord e)
  => TableValue V.Vector V.Vector (V.Vector e) Int ( IntegralType
                                                   , V.Vector Word8)
smallEnumVector =
  TableValue
    { typeValues =
        first (second $ V.map $ fromIntegral . fromEnum) . typeContainerDedup
    , generateModule = generateMonoContainer
    }

byteString ::
     TableValue V.Vector V.Vector ByteString Int (IntegralType, ByteString)
byteString =
  TableValue
    { typeValues = typeASCII
    , generateModule = \prefix -> generateASCII ASCIISpec {asCPrefix = prefix}
    }

ffiVector ::
     FFIIntegralType i
  => TableValue V.Vector V.Vector (V.Vector i) Int (IntegralType, V.Vector i)
ffiVector =
  TableValue
    {typeValues = typeContainer, generateModule = generateMonoContainer}

processTableAs ::
     (Show a, Ord bv, Ord (dv bv), SizedTy bt, VG.Vector sv a, VG.Vector dv bv)
  => TableValue sv dv a bv bt
  -> (Int, Int)
  -> ByteString
  -> sv a
  -> IO ()
{-# INLINE processTableAs #-}
processTableAs tv partitionings snakeName values = do
  generateSourcesAs tv partitionings snakeName values
  generateTests snakeName values

generateASCIITableSources ::
     (Int, Int) -> ByteString -> V.Vector ByteString -> IO ()
generateASCIITableSources partitionings snakeName values =
  R.both_
    (generateSourcesAs byteString partitionings (snakeName <> "_ptr") values)
    (generateSourcesAs
       integral
       partitionings
       (snakeName <> "_len")
       (VG.convert $ VG.map B.length values))

-- This assumes that the total length of all strings in a single
-- element is <= 255
generateASCIIVectorTableSources ::
     (Int, Int) -> ByteString -> V.Vector (V.Vector ByteString) -> IO ()
generateASCIIVectorTableSources partitionings snakeName values =
  R.both_
    (generateSourcesAs
       integral
       partitionings
       (snakeName <> "_len")
       (VG.convert $ VG.map V.length values)) $
  R.both_
    (generateSourcesAs
       ffiVector
       partitionings
       (snakeName <> "_sublens")
       (fmap
          (\v ->
             if V.null v
               then V.empty
               else V.map (toEnum :: Int -> Word8) $
                    V.scanl' (\n str -> n + B.length str) 0 v)
          values))
    (generateSourcesAs
       byteString
       partitionings
       (snakeName <> "_ptr")
       (fmap fold values))

generateSourcesAs ::
     (Ord (dv bv), Ord bv, SizedTy bt, VG.Vector dv bv, VG.Vector sv a)
  => TableValue sv dv a bv bt
  -> (Int, Int)
  -> ByteString
  -> sv a
  -> IO ()
{-# INLINEABLE generateSourcesAs #-}
generateSourcesAs tv (maxLayers, maxBits) snakeName values = do
  putStrLn $ show snakeName ++ " " ++ show p
  putStrLn $ show snakeName ++ " " ++ show cost
  let hsFile = "ucd/generated/hs/Data/UCD/Internal/" <> hsModuleName <> ".hs"
  B.writeFile (B.unpack hsFile) $
    B.unlines $
    "-- This file is automatically generated." :
    "{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}" :
    "{- HLINT ignore -}" :
    "{-# LANGUAGE MagicHash #-}" :
    ("module Data.UCD.Internal." <> hsModuleName <> " (retrieve) where\n") :
    moduleHs modul
  B.writeFile (B.unpack $ "ucd/generated/cbits/" <> snakeName <> ".c") $
    B.unlines $ "// This file is automatically generated" : moduleC modul
  where
    hsModuleName = snake2camel snakeName
    modul = generateModule tv cprefix trie
    cprefix = "_hs__ucd__" <> snakeName
    trie = first (const bty) $ typeLayers $ mkTrie bvals p
    (cost, p) =
      findOptimalPartitioning
        (itSize . findTypeForRange)
        (sizeInBytes bty)
        maxBits
        maxLayers
        bvals
    (bty, bvals) = typeValues tv values

generateTests :: (VG.Vector v a, Show a) => ByteString -> v a -> IO ()
{-# INLINEABLE generateTests #-}
generateTests snakeName values =
  withFile
    (B.unpack $ "ucd/generated/test_data/" <> snakeName <> ".txt")
    WriteMode $ \h -> VG.forM_ values $ hPrint h

snake2camel :: ByteString -> ByteString
snake2camel = B.concat . map titlecase . B.split '_'
  where
    titlecase bstr =
      case B.uncons bstr of
        Nothing -> bstr
        Just (c, cs) -> B.cons (toUpper c) cs
