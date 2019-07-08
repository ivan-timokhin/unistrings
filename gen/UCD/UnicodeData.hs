{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UCD.UnicodeData
  ( tableToVector
  , tableToNames
  , unicodeTableSize
  , Properties(..)
  , Name(Name, Unnamed)
  , BidiClass(..)
  , CompatibilityMappingTag(..)
  , NumericProperties(..)
  , fetch
  ) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad (guard)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Functor (void)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down))
import Data.Ratio (Rational, (%))
import qualified Data.Vector as V
import Data.Word (Word32, Word8)

import Data.UCD.Internal.Types
  ( BidiClass(ArabicLetter, ArabicNumber, BoundaryNeutral,
          CommonSeparator, EuropeanNumber, EuropeanSeparator,
          EuropeanTerminator, FirstStrongIsolate, LeftToRight,
          LeftToRightEmbedding, LeftToRightIsolate, LeftToRightOverride,
          NonSpacingMark, OtherNeutral, ParagraphSeparator,
          PopDirectionalFormat, PopDirectionalIsolate, RightToLeft,
          RightToLeftEmbedding, RightToLeftIsolate, RightToLeftOverride,
          SegmentSeparator, WhiteSpace)
  )
import UCD.Common (Table(Table, getTable), Range(Single, Range), tableToVector)

fetch :: IO (Table Name ByteString Properties)
fetch = do
  txt <- B.readFile "data/latest/ucd/UnicodeData.txt"
  let parsed = A.parseOnly (parser <* A.endOfInput) txt
  case parsed of
    Left err -> fail err
    Right records ->
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
    , propDecompositionMapping :: Maybe ( Maybe CompatibilityMappingTag
                                        , [Word32])
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

data CompatibilityMappingTag
  = Font
  | NoBreak
  | Initial
  | Medial
  | Final
  | Isolated
  | Circle
  | Super
  | Sub
  | Vertical
  | Wide
  | Narrow
  | Small
  | Square
  | Fraction
  | Compat
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

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
pCategory =
  A.choice
    (map
       (\(str, gc) -> gc <$ A.string str)
       [ "Lu" ~> C.UppercaseLetter
       , "Ll" ~> C.LowercaseLetter
       , "Lt" ~> C.TitlecaseLetter
       , "Lm" ~> C.ModifierLetter
       , "Lo" ~> C.OtherLetter
       , "Mn" ~> C.NonSpacingMark
       , "Mc" ~> C.SpacingCombiningMark -- TODO: UAX #44 calls this Spacing_Mark
       , "Me" ~> C.EnclosingMark
       , "Nd" ~> C.DecimalNumber
       , "Nl" ~> C.LetterNumber
       , "No" ~> C.OtherNumber
       , "Pc" ~> C.ConnectorPunctuation
       , "Pd" ~> C.DashPunctuation
       , "Ps" ~> C.OpenPunctuation
       , "Pe" ~> C.ClosePunctuation
       , "Pi" ~> C.InitialQuote -- TODO: UAX #44 calls this Initial_Punctuation
       , "Pf" ~> C.FinalQuote -- TODO: UAX #44 calls this Final_Punctuation
       , "Po" ~> C.OtherPunctuation
       , "Sm" ~> C.MathSymbol
       , "Sc" ~> C.CurrencySymbol
       , "Sk" ~> C.ModifierSymbol
       , "So" ~> C.OtherSymbol
       , "Zs" ~> C.Space
       , "Zl" ~> C.LineSeparator
       , "Zp" ~> C.ParagraphSeparator
       , "Cc" ~> C.Control
       , "Cf" ~> C.Format
       , "Cs" ~> C.Surrogate
       , "Co" ~> C.PrivateUse
       , "Cn" ~> C.NotAssigned -- TODO: UAX #44 calls this Unassigned
       ]) A.<?>
  "general category"
  where
    (~>) = (,)

pBidiClass :: A.Parser BidiClass
pBidiClass =
  A.choice
    (map (\(str, bd) -> bd <$ A.string str) $
     sortOn
       (Down . B.length . fst)
       [ "L" ~> LeftToRight
       , "R" ~> RightToLeft
       , "AL" ~> ArabicLetter
       , "EN" ~> EuropeanNumber
       , "ES" ~> EuropeanSeparator
       , "ET" ~> EuropeanTerminator
       , "AN" ~> ArabicNumber
       , "CS" ~> CommonSeparator
       , "NSM" ~> NonSpacingMark
       , "BN" ~> BoundaryNeutral
       , "B" ~> ParagraphSeparator
       , "S" ~> SegmentSeparator
       , "WS" ~> WhiteSpace
       , "ON" ~> OtherNeutral
       , "LRE" ~> LeftToRightEmbedding
       , "LRO" ~> LeftToRightOverride
       , "RLE" ~> RightToLeftEmbedding
       , "RLO" ~> RightToLeftOverride
       , "PDF" ~> PopDirectionalFormat
       , "LRI" ~> LeftToRightIsolate
       , "RLI" ~> RightToLeftIsolate
       , "FSI" ~> FirstStrongIsolate
       , "PDI" ~> PopDirectionalIsolate
       ]) A.<?>
  "bidirectional class"
  where
    (~>) = (,)

pDecompositionMapping :: A.Parser (Maybe CompatibilityMappingTag, [Word32])
pDecompositionMapping =
  (do tag <-
        optional $
        A.char '<' *>
        A.choice
          (map (\(str, t) -> t <$ A.string str) $
           sortOn
             (Down . B.length . fst)
             [ "font" ~> Font
             , "noBreak" ~> NoBreak
             , "initial" ~> Initial
             , "medial" ~> Medial
             , "final" ~> Final
             , "isolated" ~> Isolated
             , "circle" ~> Circle
             , "super" ~> Super
             , "sub" ~> Sub
             , "vertical" ~> Vertical
             , "wide" ~> Wide
             , "narrow" ~> Narrow
             , "small" ~> Small
             , "square" ~> Square
             , "fraction" ~> Fraction
             , "compat" ~> Compat
             ]) <*
        A.string "> "
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
