{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), many, optional)
import Control.Monad (guard)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Foldable (for_)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.Ratio (Rational, (%))
import Data.Word (Word32, Word8)

main :: IO ()
main = do
  records <- unidata
  printLong records

unidata :: IO [UnicodeDataRecord]
unidata = do
  txt <- B.readFile "data/latest/ucd/UnicodeData.txt"
  let parsed = A.parseOnly (unicodeData <* A.endOfInput) txt
  case parsed of
    Left err -> fail err
    Right records -> pure records

data UnicodeDataRecord =
  UDR
    { udrCode :: Word32
    , udrName :: UnicodeDataName
    , udrCategory :: C.GeneralCategory
    , udrCanonicalCombiningClass :: Word8
    , udrBidiClass :: BidiClass
    , udrDecompositionMapping :: Maybe (Maybe CompatibilityMappingTag, [Word32])
    , udrNumeric :: Maybe NumericProp
    , udrBidiMirrored :: Bool
    , udrUnicode1Name :: ByteString
    , udrSimpleUppercaseMapping :: Maybe Word32
    , udrSimpleLowercaseMapping :: Maybe Word32
    , udrSimpleTitlecaseMapping :: Maybe Word32
    }
  deriving (Eq, Show)

data UnicodeDataName
  = UName ByteString
  | Unnamed ByteString
  | RangeStart ByteString
  | RangeEnd ByteString
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
    UDR
      { udrCode = code
      , udrName = name
      , udrCategory = gc
      , udrCanonicalCombiningClass = ccc
      , udrBidiClass = bidi
      , udrDecompositionMapping = decomp
      , udrNumeric = nprops
      , udrBidiMirrored = mirrored
      , udrUnicode1Name = u1name
      , udrSimpleUppercaseMapping = uppercase
      , udrSimpleLowercaseMapping = lowercase
      , udrSimpleTitlecaseMapping = titlecase
      }
  where
    sep = void $ A.char ';'

udn :: A.Parser UnicodeDataName
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
              Nothing -> Unnamed field
    regular = UName <$> A.takeWhile1 (/= ';')

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
