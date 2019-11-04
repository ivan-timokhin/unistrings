{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Vector as V
import Data.Word (Word32, Word8)

import Data.UCD.Internal.Types
  ( BidiClass
  , DecompositionType(Canonical, Compatibility, Encircled,
                  FinalPresentationForm, Font, InitialPresentationForm,
                  IsolatedPresentationForm, MedialPresentationForm, Narrow, NoBreak,
                  Small, Squared, Subscript, Superscript, VerticalLayout,
                  VulgarFraction, Wide)
  )
import Trie (deduplicate)
import UCD.Common
  ( Range(Range, Single)
  , Table(Table, getTable)
  , enumeratedAbbrP
  , fetchGeneral
  , tableP
  , tableToVector
  )

fetch :: IO (Table Name ByteString Properties)
fetch =
  fetchGeneral "data/latest/ucd/UnicodeData.txt" $ do
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
     (Word32 -> Bool)
  -> Table annS annR Properties
  -> (V.Vector (Maybe Int), V.Vector Word32)
tableToCompositionTables exclude =
  nestedMapToTables 0 . tableToCompositionMap exclude

nestedMapToTables ::
     Ord a => a -> IntMap (IntMap a) -> (V.Vector (Maybe Int), V.Vector a)
nestedMapToTables def nmap = (topVec, bottomVec)
  where
    (topMap, bottomVecMap) = deduplicate nmap
    topVec =
      V.replicate unicodeTableSize Nothing V.// IM.toList (fmap Just topMap)
    bottomVec =
      V.concatMap
        (\m -> V.replicate unicodeTableSize def V.// IM.toList m)
        bottomVecMap

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

parser :: A.Parser [Record]
parser = many (pRecord <* A.char '\n')

pRecord :: A.Parser Record
pRecord = do
  code <- A.hexadecimal A.<?> "code point"
  sep
  ty <- pType
  sep
  gc <- pCategory
  sep
  ccc <- A.decimal A.<?> "canonical combining class"
  sep
  bidi <- pBidiClass
  sep
  decomp <- optional pDecompositionMapping
  sep
  nprops <- pNumericProperties
  sep
  mirrored <-
    True <$ A.char 'Y' <|> False <$ A.char 'N' A.<?> "bidi mirrored property"
  sep
  u1name <- A.takeWhile (/= ';')
  sep
  -- Here should have been ISO_Comment, but they've purged it from the
  -- database
  sep
  uppercase <- optional A.hexadecimal A.<?> "simple uppercase mapping"
  sep
  lowercase <- optional A.hexadecimal A.<?> "simple lowercase mapping"
  sep
  titlecase <- optional A.hexadecimal A.<?> "simple titlecase mapping"
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
    sep = void $ A.char ';'

pType :: A.Parser RecordType
pType = (special <|> regular) A.<?> "unicode character name"
  where
    special = do
      field <- A.char '<' *> A.takeWhile1 (/= '>') <* A.char '>'
      pure $
        case B.stripSuffix ", First" field of
          Just range -> RangeStart range
          Nothing ->
            case B.stripSuffix ", Last" field of
              Just range -> RangeEnd range
              Nothing -> Regular $ Unnamed field
    regular = Regular . Name <$> A.takeWhile1 (/= ';')

pCategory :: A.Parser C.GeneralCategory
pCategory = enumeratedAbbrP A.<?> "general category"

pBidiClass :: A.Parser BidiClass
pBidiClass = enumeratedAbbrP A.<?> "bidirectional class"

pDecompositionMapping :: A.Parser (DecompositionType, [Word32])
pDecompositionMapping =
  (do tag <-
        (A.char '<' *>
         tableP
           [ "font" ~> Font
           , "noBreak" ~> NoBreak
           , "initial" ~> InitialPresentationForm
           , "medial" ~> MedialPresentationForm
           , "final" ~> FinalPresentationForm
           , "isolated" ~> IsolatedPresentationForm
           , "circle" ~> Encircled
           , "super" ~> Superscript
           , "sub" ~> Subscript
           , "vertical" ~> VerticalLayout
           , "wide" ~> Wide
           , "narrow" ~> Narrow
           , "small" ~> Small
           , "square" ~> Squared
           , "fraction" ~> VulgarFraction
           , "compat" ~> Compatibility
           ] <*
         A.string "> ") <|>
        pure Canonical
      decomposition <- A.hexadecimal `A.sepBy1` A.char ' '
      pure (tag, decomposition)) A.<?>
  "decomposition mapping"
  where
    (~>) = (,)

pNumericProperties :: A.Parser (Maybe NumericProperties)
pNumericProperties =
  decimal <|>
  A.char ';' *>
  (digit <|> A.char ';' *> (numeric <|> pure Nothing)) A.<?>
  "numeric type and value"
  where
    decimal = do
      (val, rep) <- field
      _ <- A.char ';' *> rep *> A.char ';' *> rep
      pure $ Just $ Decimal val
    digit = do
      (val, rep) <- field
      _ <- A.char ';' *> rep
      pure $ Just $ Digit val
    numeric =
      fmap (Just . Numeric) $
      ((%) <$> A.signed A.decimal <* A.char '/' <*> A.decimal) <|>
      A.signed ((fromIntegral :: Integer -> Rational) <$> A.decimal)
    field = do
      val <- A.decimal
      guard $ 0 <= val && val <= 9
      pure (val, A.string $ B.pack $ show val)
