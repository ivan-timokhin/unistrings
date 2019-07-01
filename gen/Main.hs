{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (GeneralCategory(NotAssigned), toUpper)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.IO (IOMode(WriteMode), hPrint, withFile)

import Gen
  ( EnumSpec(EnumSpec, esCPrefix, esHsType, esHsTypeModule)
  , IntSpec(IntSpec, isCPrefix, isHsType)
  , Module(moduleC, moduleHs)
  , generateEnum
  , generateIntegral
  )
import Gen.Cost (SizedTy, pickBest, totalCost)
import Gen.Type
  ( EnumTy
  , IntTy
  , IntegralType(itHaskell)
  , typeEnum
  , typeIntegral
  , word8
  )
import ListM (generatePartitionings)
import Trie (BottomAnnotation, LayerAnnotation, TrieDesc, mkTrieM, partitioning)
import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  createDirectoryIfMissing True "generated/cbits"
  createDirectoryIfMissing True "generated/hs/Data/UCD/Internal"
  createDirectoryIfMissing True "generated/test_data"
  processTable
    TableDesc
      { tdSnakeName = "general_category"
      , tdType = EnumTable "GeneralCategory" "Data.Char"
      } $
    UCD.UnicodeData.tableToVector NotAssigned $
    fmap UCD.UnicodeData.propCategory records
  processTable
    TableDesc
      {tdSnakeName = "canonical_combining_class", tdType = IntegralTable word8} $
    UCD.UnicodeData.tableToVector 0 $
    fmap UCD.UnicodeData.propCanonicalCombiningClass records

processTable ::
     forall a ty.
     ( Ord a
     , SizedTy (BottomAnnotation ty)
     , SizedTy (LayerAnnotation ty)
     , Show a
     )
  => TableDesc ty a
  -> V.Vector a
  -> IO ()
processTable desc values = do
  print $ partitioning trie
  print $ totalCost trie
  let hsFile = "generated/hs/Data/UCD/Internal/" <> hsModuleName <> ".hs"
  B.writeFile (B.unpack hsFile) $
    B.unlines $
    "{-# OPTIONS_GHC -Wno-unused-imports #-}" :
    ("module Data.UCD.Internal." <> hsModuleName <> " (retrieve) where\n") :
    moduleHs modul
  B.writeFile (B.unpack $ "generated/cbits/" <> tdSnakeName desc <> ".c") $
    B.unlines $ moduleC modul
  withFile
    (B.unpack $ "generated/test_data/" <> tdSnakeName desc <> ".txt")
    WriteMode $ \h -> for_ values $ hPrint h
  where
    hsModuleName = snake2camel $ tdSnakeName desc
    modul :: Module
    modul =
      case tdType desc of
        EnumTable typ modTy ->
          generateEnum
            EnumSpec
              {esCPrefix = cprefix, esHsType = typ, esHsTypeModule = modTy}
            trie
        IntegralTable ty ->
          generateIntegral
            IntSpec {isCPrefix = cprefix, isHsType = itHaskell ty}
            trie
    cprefix = "_hs__ucd__" <> tdSnakeName desc
    trie = fromMaybe (error "Can't pick best trie") $ pickBest candidates
    candidates = map typeTrie $ partitionings >>= mkTrieM values
    typeTrie :: Foldable f => TrieDesc ann f a -> TrieDesc ty f a
    typeTrie =
      case tdType desc of
        EnumTable _ _ -> typeEnum
        IntegralTable ty -> typeIntegral ty
    partitionings = generatePartitionings 4 0 16

data TableDesc ty a =
  TableDesc
    { tdSnakeName :: ByteString
    , tdType :: TableType ty a
    }

data TableType ty a where
  EnumTable :: Enum a => ByteString -> ByteString -> TableType EnumTy a
  IntegralTable :: Integral a => IntegralType -> TableType IntTy a

snake2camel :: ByteString -> ByteString
snake2camel = B.concat . map titlecase . B.split '_'
  where
    titlecase bstr =
      case B.uncons bstr of
        Nothing -> bstr
        Just (c, cs) -> B.cons (toUpper c) cs

printLong :: Show a => [a] -> IO ()
printLong entries
  | entriesCount <= 2 * magic = print entries
  | otherwise = do
    putStrLn "["
    for_ (take magic entries) $ \e -> putStrLn $ '\t' : show e
    putStrLn "\t⋮"
    for_ (drop (entriesCount - magic) entries) $ \e -> putStrLn $ '\t' : show e
    putStrLn "]"
  where
    entriesCount = length entries
    magic = 10
