{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Codec.Archive.Zip (mkEntrySelector, sourceEntry, withArchive)
import Control.Applicative ((<|>), liftA2)
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import Data.Char (toUpper)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (find, sort)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric (showHex)
import Test.Yocto
import Text.Read (readMaybe)
import Text.XML
  ( Document
  , Element(..)
  , Name(..)
  , Node(..)
  , def
  , documentRoot
  , sinkDoc
  )

import qualified Data.UCD as UCD

main :: IO ()
main = do
  ucd <- readUCD
  let repertoire =
        fromJust $
        find ((== "repertoire") . nameLocalName . elementName) $
        elementChildren $ documentRoot ucd
      groups = elementChildren repertoire
      blocks =
        elementChildren $
        fromJust $
        find ((== "blocks") . nameLocalName . elementName) $
        elementChildren $ documentRoot ucd
  let tests =
        group_
          [ group "groups" $ groupsOnly groups : zipWith testGroup [0 ..] groups
          , group "blocks" $ map testBlock blocks
          ]
  defaultMain 100 tests

groupsOnly :: [Element] -> Suite
groupsOnly els =
  test "Only groups at top level" $
  for_ els $ \el ->
    expect "Element name should be \"group\"" $
    "group" =? nameLocalName (elementName el)

testGroup :: Int -> Element -> Suite
testGroup n elGroup = group ("Group #" ++ show n) [validRecordTypes, cpTests]
  where
    validRecordTypes =
      group "Valid record types" $
      flip map (elementChildren elGroup) $ \el ->
        test (show $ elementName el) $
        expect "Unrecognised record type" $
        predicate $
        nameLocalName (elementName el) `elem`
        ["reserved", "noncharacter", "surrogate", "char"]
    cpTests =
      group "Code point properties" $
      flip map (elementChildren elGroup) $ \el ->
        let testCP' =
              testCP (elementChildren el) $ \nm ->
                M.lookup nm (elementAttributes el) <|> M.lookup nm attrs
         in withT_ $
            case M.lookup "first-cp" (elementAttributes el) of
              Just startStr -> do
                start <- readHex startStr
                end <-
                  readHex =<<
                  requireJust
                    "Missing \"last-cp\" attribute"
                    (M.lookup "last-cp" (elementAttributes el))
                pure $ group_ $ map testCP' [toEnum start .. toEnum end]
              Nothing -> do
                cpStr <-
                  requireJust ("Unrecognized record type: " ++ show el) $
                  M.lookup "cp" $ elementAttributes el
                cp <- readHex cpStr
                pure $ testCP' $ toEnum cp
    attrs = elementAttributes elGroup

testCP :: [Element] -> (Name -> Maybe T.Text) -> UCD.CodePoint -> Suite
testCP children getAttr cp =
  group
    (show cp)
    [ test "Name aliases" $ testCPAliases children cp
    , group
        "Properties"
        [ testEnumerated "General category" "gc" UCD.generalCategory
        , test "Canonical comibing class" $ do
            xmlCCC <- canonicalCombiningClass
            expect "Canonical combining class" $
              xmlCCC =? UCD.canonicalCombiningClass cp
        , test "Name" $ do
            xmlName <- name
            expect "Name" $ xmlName =? UCD.name cp
        , testMayEnumerated "Age" "age" "unassigned" UCD.age
        , testEnumerated "Script" "sc" UCD.script
        , testMayEnumerated "Block" "blk" "NB" UCD.block
        , test "Script extensions" $ do
            xmlScriptExts <- scriptExts
            expect "Script extensions" $
              sort xmlScriptExts =? sort (UCD.scriptExtensions cp)
        , group "Boolean properties" $
          let testBoolean testName attrName f =
                test testName $ do
                  attr <- requireAttr attrName
                  b <- readBoolean attr
                  expect "Attribute value mismatch" $ b =? f cp
           in map
                (\(tn, an, f) -> testBoolean tn an f)
                [ ("White space", "WSpace", UCD.whiteSpace)
                , ("Bidi control", "Bidi_C", UCD.bidiControl)
                , ("Join control", "Join_C", UCD.joinControl)
                , ("Dash", "Dash", UCD.dash)
                , ("Quotation mark", "QMark", UCD.quotationMark)
                , ("Terminal punctuation", "Term", UCD.terminalPunctuation)
                , ("Hex digit", "Hex", UCD.hexDigit)
                , ("ASCII hex digit", "AHex", UCD.asciiHexDigit)
                , ("Ideographic", "Ideo", UCD.ideographic)
                , ("Diacritic", "Dia", UCD.diacritic)
                , ("Extender", "Ext", UCD.extender)
                , ( "Noncharacter code point"
                  , "NChar"
                  , UCD.noncharacterCodePoint)
                , ("IDS binary operator", "IDSB", UCD.idsBinaryOperator)
                , ("IDS trinary operator", "IDST", UCD.idsTrinaryOperator)
                , ("Radical", "Radical", UCD.radical)
                , ("Unified Ideograph", "UIdeo", UCD.unifiedIdeograph)
                , ("Deprecated", "Dep", UCD.deprecated)
                , ("Soft dotted", "SD", UCD.softDotted)
                , ("Logical order exception", "LOE", UCD.logicalOrderException)
                , ("Sentence terminal", "STerm", UCD.sentenceTerminal)
                , ("Variation selector", "VS", UCD.variationSelector)
                , ("Pattern white space", "Pat_WS", UCD.patternWhiteSpace)
                , ("Pattern syntax", "Pat_Syn", UCD.patternSyntax)
                , ( "Prepended concatenation mark"
                  , "PCM"
                  , UCD.prependedConcatenationMark)
                , ("Regional indicator", "RI", UCD.regionalIndicator)
                , ("Math", "Math", UCD.math)
                , ("Alphabetic", "Alpha", UCD.alphabetic)
                , ("Uppercase", "Upper", UCD.uppercase)
                , ("Lowercase", "Lower", UCD.lowercase)
                , ("Cased", "Cased", UCD.cased)
                , ("Case ignorable", "CI", UCD.caseIgnorable)
                , ("Changes when lowercased", "CWL", UCD.changesWhenLowercased)
                , ("Changes when uppercased", "CWU", UCD.changesWhenUppercased)
                , ("Changes when titlecased", "CWT", UCD.changesWhenTitlecased)
                , ("Changes when casefolded", "CWCF", UCD.changesWhenCasefolded)
                , ("Changes when casemapped", "CWCM", UCD.changesWhenCasemapped)
                , ("ID start", "IDS", UCD.idStart)
                , ("ID continue", "IDC", UCD.idContinue)
                , ("XID start", "XIDS", UCD.xidStart)
                , ("XID continue", "XIDC", UCD.xidContinue)
                , ("Default ignorable", "DI", UCD.defaultIgnorableCodePoint)
                , ("Grapheme extend", "Gr_Ext", UCD.graphemeExtend)
                , ("Grapheme base", "Gr_Base", UCD.graphemeBase)
                , ( "Changes when NFKC casefolded"
                  , "CWKCF"
                  , UCD.changesWhenNFKCCasefolded)
                , ("Bidi Mirrored", "Bidi_M", UCD.bidiMirrored)
                ]
        , testMayEnumerated
            "Hangul syllable type"
            "hst"
            "NA"
            UCD.hangulSyllableType
        , group "Simple case mappings" $
          let testCPProp testName attrName f =
                test testName $ do
                  attr <- requireAttr attrName
                  expected <-
                    if attr == "#"
                      then pure (fromEnum cp)
                      else readHex attr
                  expect "Attribute value mismatch" $
                    expected =? fromEnum (f cp)
           in map
                (\(tn, an, f) -> testCPProp tn an f)
                [ ("Lowercase", "slc", UCD.simpleLowercaseMapping)
                , ("Uppercase", "suc", UCD.simpleUppercaseMapping)
                , ("Titlecase", "stc", UCD.simpleTitlecaseMapping)
                , ("Folding", "scf", UCD.simpleCaseFolding)
                ]
        , group "Full case mappings" $
          let testCaseMappingProp testName attrName f =
                test testName $ do
                  attr <- requireAttr attrName
                  expected <-
                    if attr == "#"
                      then pure [fromEnum cp]
                      else traverse readHex (T.words attr)
                  let actual =
                        case f cp of
                          UCD.SingleCM c1 -> [fromEnum c1]
                          UCD.DoubleCM c1 c2 -> [fromEnum c1, fromEnum c2]
                          UCD.TripleCM c1 c2 c3 ->
                            [fromEnum c1, fromEnum c2, fromEnum c3]
                  expect "Attribute value mismatch" $ expected =? actual
           in map
                (\(tn, an, f) -> testCaseMappingProp tn an f)
                [ ("Lowercase", "lc", UCD.lowercaseMapping)
                , ("Uppercase", "uc", UCD.uppercaseMapping)
                , ("Titlecase", "tc", UCD.titlecaseMapping)
                , ("Folding", "cf", UCD.caseFolding)
                ]
        , group
            "Numeric properties"
            [ test "Type" $ do
                xmlTy <- requireAttr "nt"
                let ucdTy =
                      case UCD.numeric cp of
                        Nothing -> "None"
                        Just n ->
                          case n of
                            UCD.Decimal _ -> "De"
                            UCD.Digit _ -> "Di"
                            UCD.Numeric _ -> "Nu"
                expect "Attribute mismatch" $ xmlTy =? ucdTy
            , test "Value" $ do
                xmlVal <-
                  requireAttr "nv" >>= \case
                    "NaN" -> pure Nothing
                    str -> do
                      let (numStr, denomStr) = break (== '/') $ T.unpack str
                      num <-
                        requireJust ("Numerator " ++ show numStr) $
                        readMaybe numStr
                      denom <-
                        case denomStr of
                          "" -> pure 1
                          (_:dstr) ->
                            requireJust ("Denominator " ++ show dstr) $
                            readMaybe dstr
                      pure $ Just $ num % denom
                let ucdVal =
                      UCD.numeric cp <&> \case
                        UCD.Decimal v -> fromIntegral v
                        UCD.Digit v -> fromIntegral v
                        UCD.Numeric v -> v
                expect "Attribute mismatch" $ xmlVal =? ucdVal
            ]
        , testMayEnumerated
            "Decomposition type"
            "dt"
            "none"
            UCD.decompositionType
        , test "Decomposition" $ do
            let decompType = UCD.decompositionType cp
            decompMap <-
              requireAttr "dm" >>= \case
                "#" -> pure [cp]
                str -> traverse (fmap toEnum . readHex) $ T.words str
            case decompType of
              Just UCD.Canonical ->
                expect "Canonical decomposition" $
                foldMap UCD.canonicalDecomposition decompMap =?
                UCD.nontrivialCanonicalDecomposition cp
              _ ->
                expect "Canonical decomposition" $
                [] =? UCD.nontrivialCanonicalDecomposition cp
            case decompType of
              Nothing ->
                expect "Compatibility decomposition" $
                [] =? UCD.nontrivialCompatibilityDecomposition cp
              _ ->
                expect "Compatibility decomposition" $
                foldMap UCD.compatibilityDecomposition decompMap =?
                UCD.nontrivialCompatibilityDecomposition cp
        , group "Normalisation quick check" $
          let getQuickCheck attr =
                case getAttr attr of
                  Just "Y" -> pure $ Just True
                  Just "N" -> pure $ Just False
                  Just "M" -> pure Nothing
                  _ -> criticalFailure "Can't parse quick check attribute"
           in [ test "NFD" $ do
                  nfdQC <- getQuickCheck "NFD_QC"
                  expect "Attribute value mismatch" $
                    nfdQC =? Just (UCD.nfdQuickCheck cp)
              , test "NFC" $ do
                  nfcQC <- getQuickCheck "NFC_QC"
                  expect "Attribute value mismatch" $
                    nfcQC =? UCD.nfcQuickCheck cp
              , test "NFKD" $ do
                  nfkdQC <- getQuickCheck "NFKD_QC"
                  expect "Attribute value mismatch" $
                    nfkdQC =? Just (UCD.nfkdQuickCheck cp)
              , test "NFKC" $ do
                  nfkcQC <- getQuickCheck "NFKC_QC"
                  expect "Attribute value mismatch" $
                    nfkcQC =? UCD.nfkcQuickCheck cp
              ]
        , test "NFKC case fold" $ do
            nfkcCF <-
              requireAttr "NFKC_CF" >>= \case
                "#" -> pure [cp]
                str -> traverse (fmap toEnum . readHex) $ T.words str
            case UCD.nfkcCaseFold cp of
              UCD.ShortCF c -> expect "Value mismatch" $ nfkcCF =? [c]
              UCD.LongCF cs -> do
                expect "LongCF for long strings" $ 1 /=? length cs
                expect "Value mismatch" $ nfkcCF =? cs
        , testEnumerated "Joining type" "jt" UCD.joiningType
        , testMayEnumerated
            "Joining group"
            "jg"
            "No_Joining_Group"
            UCD.joiningGroup
        , testEnumerated "Vertical orientation" "vo" UCD.verticalOrientation
        , testEnumerated "Line break" "lb" UCD.lineBreak
        , testEnumerated "Grapheme cluster break" "GCB" UCD.graphemeClusterBreak
        , testEnumerated "Sentence break" "SB" UCD.sentenceBreak
        , testEnumerated "Word break" "WB" UCD.wordBreak
        , testEnumerated "East Asian width" "ea" UCD.eastAsianWidth
        , testEnumerated "Bidi class" "bc" UCD.bidiClass
        , test "Bidi mirroring glych" $ do
            bmg <-
              requireAttr "bmg" >>= \case
                "" -> pure Nothing
                str -> Just . toEnum <$> readHex str
            expect "Bidi mirroring glyph" $ bmg =? UCD.bidiMirroringGlyph cp
        , testMayEnumerated
            "Bidi Paired Bracket Type"
            "bpt"
            "n"
            UCD.bidiPairedBracketType
        , test "Bidi paired bracket" $ do
            bpbText <- requireAttr "bpb"
            bpbVal <-
              case bpbText of
                "#" -> pure cp
                _ -> toEnum <$> readHex bpbText
            expect "Bidi Paired Bracket" $ bpbVal =? UCD.bidiPairedBracket cp
        , test "Equivalent unified ideograph" $ do
            eui <- traverse readHex $ getAttr "EqUIdeo"
            expect "Attribute value mismatch" $
              eui =? (fromEnum <$> UCD.equivalentUnifiedIdeograph cp)
        , test "Unicode 1 Name" $ do
            u1name <- requireAttr "na1"
            expect "Name mismatch" $ TE.encodeUtf8 u1name =? UCD.unicode1Name cp
        , testMayEnumerated
            "Indic positional category"
            "InPC"
            "NA"
            UCD.indicPositionalCategory
        ]
    ]
  where
    canonicalCombiningClass = do
      cccText <- requireAttr "ccc"
      requireJust ("Can't parse canonical combining class " ++ show cccText) $
        readMaybe $ T.unpack cccText
    name = TE.encodeUtf8 . T.replace "#" hexStr <$> requireAttr "na"
    hexStr =
      T.pack $
      let raw = map toUpper $ showHex (fromEnum cp) ""
       in if length raw < 4
            then replicate (4 - length raw) ' ' ++ raw
            else raw
    scriptExts =
      traverse (parseEnumerated "script extension") . T.words =<<
      requireAttr "scx"
    testEnumerated ::
         (Show p, Eq p, UCD.EnumeratedProperty p)
      => String
      -> Name
      -> (UCD.CodePoint -> p)
      -> Suite
    testEnumerated testName attrName f =
      test testName $ do
        attr <- requireAttr attrName
        xmlVal <- parseEnumerated testName attr
        expect "Attribute value mismatch" $ xmlVal =? f cp
    testMayEnumerated ::
         (Show p, Eq p, UCD.EnumeratedProperty p)
      => String
      -> Name
      -> T.Text
      -> (UCD.CodePoint -> Maybe p)
      -> Suite
    testMayEnumerated testName attrName absent f =
      test testName $ do
        attr <- requireAttr attrName
        if attr == absent
          then expect "Attribute should be absent" $ Nothing =? f cp
          else do
            xmlVal <- parseEnumerated testName attr
            expect "Attribute value mismatch" $ Just xmlVal =? f cp
    requireAttr :: Name -> Test T.Text
    requireAttr attrName =
      requireJust
        ("Can't locate attribute " ++ show attrName)
        (getAttr attrName)
    parseEnumerated :: UCD.EnumeratedProperty p => String -> T.Text -> Test p
    parseEnumerated propertyName txt =
      requireJust ("Can't parse " ++ propertyName ++ ": " ++ show txt) $
      find
        ((== txt8) . toUpper8 . UCD.abbreviatedPropertyValueName)
        [minBound .. maxBound]
      where
        txt8 = toUpper8 $ TE.encodeUtf8 txt
    toUpper8 = B.map (.|. 0x20)

testCPAliases :: [Element] -> UCD.CodePoint -> Test ()
testCPAliases children cp = do
  aliases <-
    sort <$> traverse (\elt -> liftA2 (,) (aliasType elt) (alias elt)) aliasElts
  expect "Name aliases" $ aliases =? UCD.nameAliases cp
  where
    aliasElts =
      filter
        (\child -> nameLocalName (elementName child) == "name-alias")
        children
    aliasType elt =
      case M.lookup "type" (elementAttributes elt) of
        Just "correction" -> pure UCD.CorrectionAlias
        Just "control" -> pure UCD.ControlAlias
        Just "alternate" -> pure UCD.AlternateAlias
        Just "figment" -> pure UCD.FigmentAlias
        Just "abbreviation" -> pure UCD.AbbreviationAlias
        Just otherStr ->
          criticalFailure $ "Unrecognised name alias type" ++ show otherStr
        Nothing -> criticalFailure "Can't locate alias type"
    alias elt =
      case M.lookup "alias" (elementAttributes elt) of
        Just str -> pure $ TE.encodeUtf8 str
        Nothing -> criticalFailure "Can't locate alias"

testBlock :: Element -> Suite
testBlock blockDescr =
  test (T.unpack blockName) $ do
    start <- (toEnum :: Int -> UCD.CodePoint) <$> blockStart
    end <- toEnum <$> blockEnd
    for_ [start .. end] $ \cp -> expect (show cp) $ block =? Just (UCD.block cp)
  where
    blockName = fromJust $ M.lookup "name" $ elementAttributes blockDescr
    blockStart =
      case M.lookup "first-cp" $ elementAttributes blockDescr of
        Just str -> readHex str
        Nothing -> criticalFailure "Can't locate block start"
    blockEnd =
      case M.lookup "last-cp" $ elementAttributes blockDescr of
        Just str -> readHex str
        Nothing -> criticalFailure "Can't locate block end"
    block
      | blockName == "No_Block" = Just Nothing
      | otherwise =
        Just <$> flip find [minBound .. maxBound] $ \b ->
          let prepare =
                T.toLower . T.filter (\c -> c /= ' ' && c /= '-' && c /= '\'')
              bname = prepare $ T.dropEnd 5 $ T.pack $ show b
           in bname == prepare blockName

readHex :: HasCallStack => T.Text -> Test Int
{-# INLINE readHex #-}
readHex t =
  withFrozenCallStack $
  case TR.hexadecimal t of
    Left err -> criticalFailure $ "Reading " ++ show t ++ ": " ++ err
    Right (n, "") -> pure n
    Right (_, rest) ->
      criticalFailure $ "Leftover parse input " ++ show (T.unpack rest)

readBoolean :: HasCallStack => T.Text -> Test Bool
{-# INLINE readBoolean #-}
readBoolean "Y" = pure True
readBoolean "N" = pure False
readBoolean txt =
  withFrozenCallStack $ criticalFailure $ "Unable to read boolean: " ++ show txt

readUCD :: IO Document
readUCD = do
  selector <- mkEntrySelector "ucd.nounihan.grouped.xml"
  withArchive "data/latest/ucdxml/ucd.nounihan.grouped.zip" $
    sourceEntry selector (sinkDoc def)

elementChildren :: Element -> [Element]
elementChildren =
  mapMaybe
    (\case
       NodeElement el -> Just el
       _ -> Nothing) .
  elementNodes
