{-
Copyright 2019 Ivan Timokhin

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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module UCD.UnicodeData
  ( tableToVector
  , tableToNames
  , tableToDecompositionVector
  , tableToCompositionTables
  , unicodeTableSize
  , Properties(..)
  , Name(Name, Unnamed)
  , NumericProperties(..)
  , fetch
  ) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Char (toLower)
import Data.Foldable (foldl')
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Word (Word32, Word8)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import Data.Unistring.UCD.Internal.Types
  ( BidiClass
  , DecompositionType(Canonical)
  )
import Trie (deduplicate)
import UCD.Common
  ( Parser_
  , Range(Range, Single)
  , Table(Table, getTable)
  , char8
  , enumeratedAbbrP
  , fetchGeneral
  , semicolon
  , tableP
  , tableToVector
  )

fetch :: IO (Table Name ByteString Properties)
fetch =
  fetchGeneral "UnicodeData.txt" $ do
    records <- parser
    case rangeify records of
      Left err -> fail err
      Right ranges -> pure $ Table ranges

tableToNames :: Table Name annR a -> V.Vector ByteString
tableToNames table =
  V.replicate unicodeTableSize "" V.//
  mapMaybe
    (\case
       Single cp (Name n) _ -> Just (fromIntegral cp, n)
       _ -> Nothing)
    (getTable table)

-- Note that the resulting vectors has empty entries for codepoints
-- with trivial decompositions (to itself), so that we don't end up
-- storing all 0x110000 decompositions in the database
tableToDecompositionVector ::
     Bool -> Table annS annR Properties -> V.Vector (V.Vector Int32)
tableToDecompositionVector compat table = decompositions
  where
    decompositions =
      flip V.map mappings $ \case
        Nothing -> V.empty
        Just mapping ->
          foldMap
            (\i ->
               let d = decompositions V.! fromIntegral i
                in if V.null d
                     then V.singleton $ fromIntegral i
                     else d)
            mapping
    mappings =
      flip V.map (tableToVector Nothing $ fmap propDecompositionMapping table) $ \case
        Nothing -> Nothing
        Just (ty, mapping)
          | compat || ty == Canonical -> Just mapping
          | otherwise -> Nothing

tableToCompositionTables ::
     VG.Vector v Word32
  => (Word32 -> Bool)
  -> Table annS annR Properties
  -> (V.Vector (Maybe Int), v Word32)
tableToCompositionTables exclude =
  nestedMapToTables 0 . tableToCompositionMap exclude

nestedMapToTables ::
     (VG.Vector v a, Ord a)
  => a
  -> IntMap (IntMap a)
  -> (V.Vector (Maybe Int), v a)
nestedMapToTables def nmap = (topVec, bottomVec)
  where
    (topMap, bottomVecMap) = deduplicate nmap
    topVec =
      V.replicate unicodeTableSize Nothing V.// IM.toList (fmap Just topMap)
    bottomMapsCount = V.length bottomVecMap
    bottomVec =
      VG.replicate (unicodeTableSize * bottomMapsCount) def VG.//
      [ (tableNo * unicodeTableSize + i, v)
      | (tableNo, table) <- V.toList (V.indexed bottomVecMap)
      , (i, v) <- IM.toList table
      ]

tableToCompositionMap ::
     (Word32 -> Bool) -> Table annS annR Properties -> IntMap (IntMap Word32)
tableToCompositionMap exclude =
  foldl' insert IM.empty . mapMaybe getSingle . getTable
  where
    insert ::
         IntMap (IntMap Word32)
      -> (Word32, Properties)
      -> IntMap (IntMap Word32)
    insert table (cp, Properties {propDecompositionMapping = Just (Canonical, [d1, d2])})
      | not (exclude cp) =
        IM.insertWith
          IM.union
          (fromIntegral d1)
          (IM.singleton (fromIntegral d2) cp)
          table
    insert table _ = table
    getSingle :: Range annS annR a -> Maybe (Word32, a)
    getSingle (Single cp _ a) = Just (cp, a)
    getSingle _ = Nothing

unicodeTableSize :: Int
unicodeTableSize = 0x110000

rangeify :: [Record] -> Either String [Range Name ByteString Properties]
rangeify [] = Right []
rangeify (Record (Regular uname) code datum:rest) =
  (Single code uname datum :) <$> rangeify rest
rangeify (start@(Record (RangeStart rname1) code1 datum1):end@(Record (RangeEnd rname2) code2 datum2):rest)
  | rname1 == rname2 && datum1 == datum2 =
    (Range code1 code2 rname1 datum1 :) <$> rangeify rest
  | otherwise =
    Left $ "Mismatch between\n" ++ show start ++ "\nand\n" ++ show end
rangeify (record:_) = Left $ "Unpaired record " ++ show record

data Record =
  Record
    { recordType :: RecordType
    , recordCode :: Word32
    , recordData :: Properties
    }
  deriving (Eq, Show)

data Properties =
  Properties
    { propCategory :: C.GeneralCategory
    , propCanonicalCombiningClass :: Word8
    , propBidiClass :: BidiClass
    , propDecompositionMapping :: Maybe (DecompositionType, [Word32])
    , propNumeric :: Maybe NumericProperties
    , propBidiMirrored :: Bool
    , propUnicode1Name :: ByteString
    , propSimpleUppercaseMapping :: Maybe Word32
    , propSimpleLowercaseMapping :: Maybe Word32
    , propSimpleTitlecaseMapping :: Maybe Word32
    }
  deriving (Eq, Show)

data RecordType
  = Regular Name
  | RangeStart ByteString
  | RangeEnd ByteString
  deriving (Eq, Show)

data Name
  = Name ByteString
  | Unnamed ByteString
  deriving (Eq, Show)

data NumericProperties
  = Decimal Word8
  | Digit Word8
  | Numeric Rational
  deriving (Eq, Show)

parser :: Parser_ [Record]
parser = many (pRecord <* MB.eol)

pRecord :: Parser_ Record
pRecord = do
  code <- MBL.hexadecimal M.<?> "code point"
  sep
  ty <- pType
  sep
  gc <- pCategory
  sep
  ccc <- MBL.decimal M.<?> "canonical combining class"
  sep
  bidi <- pBidiClass
  sep
  decomp <- optional pDecompositionMapping
  sep
  nprops <- pNumericProperties
  sep
  mirrored <-
    True <$ char8 'Y' <|> False <$ char8 'N' M.<?> "bidi mirrored property"
  sep
  u1name <-
    M.takeWhileP (Just "Unicode 1 Name") (/= fromIntegral (fromEnum ';'))
  sep
  -- Here should have been ISO_Comment, but they've purged it from the
  -- database
  sep
  uppercase <- optional MBL.hexadecimal M.<?> "simple uppercase mapping"
  sep
  lowercase <- optional MBL.hexadecimal M.<?> "simple lowercase mapping"
  sep
  titlecase <- optional MBL.hexadecimal M.<?> "simple titlecase mapping"
  pure $
    Record
      { recordType = ty
      , recordCode = code
      , recordData =
          Properties
            { propCategory = gc
            , propCanonicalCombiningClass = ccc
            , propBidiClass = bidi
            , propDecompositionMapping = decomp
            , propNumeric = nprops
            , propBidiMirrored = mirrored
            , propUnicode1Name = u1name
            , propSimpleUppercaseMapping = uppercase
            , propSimpleLowercaseMapping = lowercase
            , propSimpleTitlecaseMapping = titlecase
            }
      }
  where
    sep = semicolon

pType :: Parser_ RecordType
pType = (special <|> regular) M.<?> "unicode character name"
  where
    special = do
      field <-
        char8 '<' *>
        M.takeWhile1P (Just "Range name") (/= fromIntegral (fromEnum '>')) <*
        char8 '>'
      pure $
        case B.stripSuffix ", First" field of
          Just range -> RangeStart range
          Nothing ->
            case B.stripSuffix ", Last" field of
              Just range -> RangeEnd range
              Nothing -> Regular $ Unnamed field
    regular =
      Regular . Name <$>
      M.takeWhile1P (Just "Name") (/= fromIntegral (fromEnum ';'))

pCategory :: Parser_ C.GeneralCategory
pCategory = enumeratedAbbrP M.<?> "general category"

pBidiClass :: Parser_ BidiClass
pBidiClass = enumeratedAbbrP M.<?> "bidirectional class"

pDecompositionMapping :: Parser_ (DecompositionType, [Word32])
pDecompositionMapping =
  (do tag <-
        (char8 '<' *>
         tableP
           (map
              (\val -> (B.pack $ lowerFst $ show val, val))
              [minBound .. maxBound]) <*
         MB.string "> ") <|>
        pure Canonical
      decomposition <- MBL.hexadecimal `M.sepBy1` char8 ' '
      pure (tag, decomposition)) M.<?>
  "decomposition mapping"
  where
    lowerFst (c:cs) = toLower c : cs
    lowerFst [] = []

pNumericProperties :: Parser_ (Maybe NumericProperties)
pNumericProperties =
  decimal <|>
  semicolon *>
  (digit <|> semicolon *> (numeric <|> pure Nothing)) M.<?>
  "numeric type and value"
  where
    decimal = do
      (val, rep) <- field
      _ <- semicolon *> rep *> semicolon *> rep
      pure $ Just $ Decimal val
    digit = do
      (val, rep) <- field
      _ <- semicolon *> rep
      pure $ Just $ Digit val
    numeric =
      fmap (Just . Numeric) $ do
        numerator <- MBL.signed (pure ()) MBL.decimal
        denominator <- (char8 '/' *> MBL.decimal) <|> pure 1
        pure $ numerator % denominator
    field = do
      val <- MBL.decimal
      guard $ 0 <= val && val <= 9
      pure (val, MB.string $ B.pack $ show val)
