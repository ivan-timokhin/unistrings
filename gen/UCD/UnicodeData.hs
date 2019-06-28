{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UCD.UnicodeData
  ( UnicodeDataRange(Single, Range)
  , generalCategoryVector
  , unicodeTableSize
  , UnicodeData(..)
  , UnicodeName(UName, Unnamed)
  , BidiClass(..)
  , CompatibilityMappingTag(..)
  , unidata
  ) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad (guard)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Functor (void)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.Ratio (Rational, (%))
import qualified Data.Vector as V
import Data.Word (Word32, Word8)

unidata :: IO [UnicodeDataRange]
unidata = do
  txt <- B.readFile "data/latest/ucd/UnicodeData.txt"
  let parsed = A.parseOnly (unicodeData <* A.endOfInput) txt
  case parsed of
    Left err -> fail err
    Right records ->
      case rangeify records of
        Left err -> fail err
        Right ranges -> pure ranges

generalCategoryVector :: [UnicodeDataRange] -> V.Vector C.GeneralCategory
generalCategoryVector ranges =
  V.replicate unicodeTableSize C.NotAssigned V.// assignments
  where
    assignments =
      ranges >>= \case
        Single code _ udata -> [(fromIntegral code, udCategory udata)]
        Range lo hi _ udata ->
          [(fromIntegral i, udCategory udata) | i <- [lo .. hi]]

unicodeTableSize :: Int
unicodeTableSize = 0x110000

data UnicodeDataRange
  = Single Word32 UnicodeName UnicodeData
  | Range Word32 Word32 ByteString UnicodeData
  deriving (Eq, Show)

rangeify :: [UnicodeDataRecord] -> Either String [UnicodeDataRange]
rangeify [] = Right []
rangeify (UDRecord (Regular uname) code datum:rest) =
  (Single code uname datum :) <$> rangeify rest
rangeify (start@(UDRecord (RangeStart rname1) code1 datum1):end@(UDRecord (RangeEnd rname2) code2 datum2):rest)
  | rname1 == rname2 && datum1 == datum2 =
    (Range code1 code2 rname1 datum1 :) <$> rangeify rest
  | otherwise =
    Left $ "Mismatch between\n" ++ show start ++ "\nand\n" ++ show end
rangeify (record:_) = Left $ "Unpaired record " ++ show record

data UnicodeDataRecord =
  UDRecord
    { udrName :: UnicodeDataRecordType
    , udrCode :: Word32
    , udrData :: UnicodeData
    }
  deriving (Eq, Show)

data UnicodeData =
  UData
    { udCategory :: C.GeneralCategory
    , udCanonicalCombiningClass :: Word8
    , udBidiClass :: BidiClass
    , udDecompositionMapping :: Maybe (Maybe CompatibilityMappingTag, [Word32])
    , udNumeric :: Maybe NumericProp
    , udBidiMirrored :: Bool
    , udUnicode1Name :: ByteString
    , udSimpleUppercaseMapping :: Maybe Word32
    , udSimpleLowercaseMapping :: Maybe Word32
    , udSimpleTitlecaseMapping :: Maybe Word32
    }
  deriving (Eq, Show)

data UnicodeDataRecordType
  = Regular UnicodeName
  | RangeStart ByteString
  | RangeEnd ByteString
  deriving (Eq, Show)

data UnicodeName
  = UName ByteString
  | Unnamed ByteString
  deriving (Eq, Show)

data BidiClass
  = LeftToRight
  | RightToLeft
  | ArabicLetter
  | EuropeanNumber
  | EuropeanSeparator
  | EuropeanTerminator
  | ArabicNumber
  | CommonSeparator
  | NonSpacingMark
  | BoundaryNeutral
  | ParagraphSeparator
  | SegmentSeparator
  | WhiteSpace
  | OtherNeutral
  | LeftToRightEmbedding
  | LeftToRightOverride
  | RightToLeftEmbedding
  | RightToLeftOverride
  | PopDirectionalFormat
  | LeftToRightIsolate
  | RightToLeftIsolate
  | FirstStrongIsolate
  | PopDirectionalIsolate
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

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

data NumericProp
  = Decimal Word8
  | Digit Word8
  | Numeric Rational
  deriving (Eq, Show)

unicodeData :: A.Parser [UnicodeDataRecord]
unicodeData = many (udr <* A.char '\n')

udr :: A.Parser UnicodeDataRecord
udr = do
  code <- A.hexadecimal A.<?> "code point"
  sep
  name <- udn
  sep
  gc <- ugc
  sep
  ccc <- A.decimal A.<?> "canonical combining class"
  sep
  bidi <- bidiClass
  sep
  decomp <- optional decompMapping
  sep
  nprops <- numericProp
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
    UDRecord
      { udrName = name
      , udrCode = code
      , udrData =
          UData
            { udCategory = gc
            , udCanonicalCombiningClass = ccc
            , udBidiClass = bidi
            , udDecompositionMapping = decomp
            , udNumeric = nprops
            , udBidiMirrored = mirrored
            , udUnicode1Name = u1name
            , udSimpleUppercaseMapping = uppercase
            , udSimpleLowercaseMapping = lowercase
            , udSimpleTitlecaseMapping = titlecase
            }
      }
  where
    sep = void $ A.char ';'

udn :: A.Parser UnicodeDataRecordType
udn = (special <|> regular) A.<?> "unicode character name"
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
    regular = Regular . UName <$> A.takeWhile1 (/= ';')

ugc :: A.Parser C.GeneralCategory
ugc =
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

bidiClass :: A.Parser BidiClass
bidiClass =
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

decompMapping :: A.Parser (Maybe CompatibilityMappingTag, [Word32])
decompMapping =
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

numericProp :: A.Parser (Maybe NumericProp)
numericProp =
  decimal <|>
  A.char ';' *>
  (digit <|> A.char ';' *> (numeric <|> pure Nothing)) A.<?>
  "numeric type and value"
  where
    decimal = do
      val <- A.decimal
      guard $ 0 <= val && val <= 9
      let strval = B.pack $ show val
      _ <- A.char ';' *> A.string strval *> A.char ';' *> A.string strval
      pure $ Just $ Decimal val
    digit = do
      val <- A.decimal
      guard $ 0 <= val && val <= 9
      let strval = B.pack $ show val
      _ <- A.char ';' *> A.string strval
      pure $ Just $ Digit val
    numeric =
      fmap (Just . Numeric) $
      ((%) <$> A.signed A.decimal <* A.char '/' <*> A.decimal) <|>
      A.signed ((fromIntegral :: Integer -> Rational) <$> A.decimal)
