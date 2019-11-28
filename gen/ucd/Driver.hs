{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Driver where

import Control.Arrow ((&&&))
import Control.Concurrent.Async (concurrently_)
import Data.Bifunctor (first, second)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Foldable (fold, for_)
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

typeName ::
     forall a. Typeable a
  => ByteString
typeName = B.pack $ tyConName $ typeRepTyCon $ typeRep $ Proxy @a

moduleName ::
     forall a. Typeable a
  => ByteString
moduleName = B.pack $ tyConModule $ typeRepTyCon $ typeRep $ Proxy @a

data TableValue a bv bt =
  TableValue
    { typeValues :: V.Vector a -> (bt, V.Vector bv)
    , generateModule :: ByteString -> TrieDesc Identity IntegralType bt bv -> Module
    }

enum ::
     forall e. (Enum e, Typeable e)
  => TableValue e e IntegralType
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
  => TableValue (Maybe e) (Maybe e) IntegralType
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
     forall i. (Integral i, Typeable i)
  => TableValue i i IntegralType
integral =
  TableValue
    { typeValues = typeIntegral &&& id
    , generateModule =
        \prefix ->
          generateIntegral IntSpec {isCPrefix = prefix, isHsType = typeName @i}
    }

maybeIntegral ::
     forall i. (Integral i, Typeable i)
  => TableValue (Maybe i) (Maybe i) IntegralType
maybeIntegral =
  TableValue
    { typeValues = typeMIntegral &&& id
    , generateModule =
        \prefix ->
          generateMayIntegral IntSpec {isCPrefix = prefix, isHsType = "Int"}
    }

bool :: TableValue Bool Bool IntegralType
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

maybeBool :: TableValue (Maybe Bool) (Maybe Bool) IntegralType
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
  => TableValue (V.Vector e) Int (IntegralType, V.Vector Word8)
smallEnumVector =
  TableValue
    { typeValues =
        first (second $ V.map $ fromIntegral . fromEnum) . typeContainerDedup
    , generateModule = generateMonoContainer
    }

byteString :: TableValue ByteString Int (IntegralType, ByteString)
byteString =
  TableValue
    { typeValues = typeASCII
    , generateModule = \prefix -> generateASCII ASCIISpec {asCPrefix = prefix}
    }

ffiVector ::
     FFIIntegralType i => TableValue (V.Vector i) Int (IntegralType, V.Vector i)
ffiVector =
  TableValue
    {typeValues = typeContainer, generateModule = generateMonoContainer}

processTableAs ::
     (Show a, Ord bv, SizedTy bt)
  => TableValue a bv bt
  -> (Int, Int)
  -> ByteString
  -> V.Vector a
  -> IO ()
{-# INLINE processTableAs #-}
processTableAs tv partitionings snakeName values = do
  generateSourcesAs tv partitionings snakeName values
  generateTests snakeName values

generateASCIITableSources ::
     (Int, Int) -> ByteString -> V.Vector ByteString -> IO ()
generateASCIITableSources partitionings snakeName values =
  concurrently_
    (generateSourcesAs byteString partitionings (snakeName <> "_ptr") values)
    (generateSourcesAs
       integral
       partitionings
       (snakeName <> "_len")
       (fmap B.length values))

-- This assumes that the total length of all strings in a single
-- element is <= 255
generateASCIIVectorTableSources ::
     (Int, Int) -> ByteString -> V.Vector (V.Vector ByteString) -> IO ()
generateASCIIVectorTableSources partitionings snakeName values =
  concurrently_
    (generateSourcesAs
       integral
       partitionings
       (snakeName <> "_len")
       (fmap V.length values)) $
  concurrently_
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
     (Ord bv, SizedTy bt)
  => TableValue a bv bt
  -> (Int, Int)
  -> ByteString
  -> V.Vector a
  -> IO ()
{-# INLINEABLE generateSourcesAs #-}
generateSourcesAs tv (maxLayers, maxBits) snakeName values = do
  putStrLn $ show snakeName ++ " " ++ show p
  putStrLn $ show snakeName ++ " " ++ show cost
  let hsFile = "ucd/generated/hs/Data/UCD/Internal/" <> hsModuleName <> ".hs"
  B.writeFile (B.unpack hsFile) $
    B.unlines $
    "{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}" :
    "{- HLINT ignore -}" :
    "{-# LANGUAGE MagicHash #-}" :
    ("module Data.UCD.Internal." <> hsModuleName <> " (retrieve) where\n") :
    moduleHs modul
  B.writeFile (B.unpack $ "ucd/generated/cbits/" <> snakeName <> ".c") $
    B.unlines $ moduleC modul
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

generateTests :: Show a => ByteString -> V.Vector a -> IO ()
{-# INLINEABLE generateTests #-}
generateTests snakeName values =
  withFile
    (B.unpack $ "ucd/generated/test_data/" <> snakeName <> ".txt")
    WriteMode $ \h -> for_ values $ hPrint h

snake2camel :: ByteString -> ByteString
snake2camel = B.concat . map titlecase . B.split '_'
  where
    titlecase bstr =
      case B.uncons bstr of
        Nothing -> bstr
        Just (c, cs) -> B.cons (toUpper c) cs
