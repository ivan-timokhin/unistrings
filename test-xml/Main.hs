{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Archive.Zip (mkEntrySelector, sourceEntry, withArchive)
import Control.Applicative ((<|>), liftA2)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import Data.Char (chr, toUpper)
import Data.Foldable (for_, traverse_)
import Data.List (find, sort)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Ratio ((%), denominator, numerator)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Numeric (showHex)
import Test.Yocto
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
        group
          ""
          [ group "groups" $ groupsOnly groups : zipWith testGroup [0 ..] groups
          , group "blocks" $ map testBlock blocks
          ]
  defaultMain tests

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
      flip concatMap (elementChildren elGroup) $ \el ->
        let testCP' =
              testCP (elementChildren el) $ \nm ->
                M.lookup nm (elementAttributes el) <|> M.lookup nm attrs
         in case M.lookup "first-cp" (elementAttributes el) of
              Just startStr ->
                let start = readHex startStr
                    end =
                      readHex $
                      fromMaybe
                        (error "Missing \"last-cp\" attribute")
                        (M.lookup "last-cp" (elementAttributes el))
                 in map testCP' [toEnum start .. toEnum end]
              Nothing ->
                case M.lookup "cp" (elementAttributes el) of
                  Just cpStr ->
                    let cp = readHex cpStr
                     in [testCP' $ toEnum cp]
                  Nothing -> error $ "Unrecognized record type: " ++ show el
    attrs = elementAttributes elGroup

testCP :: [Element] -> (Name -> Maybe T.Text) -> UCD.CodePoint -> Suite
testCP children getAttr cp =
  group
    (show cp)
    [ test "Name aliases" $ testCPAliases children cp
    , test "Properties" $ do
        testEnumerated "General category" "gc" UCD.generalCategory
        xmlCCC <- canonicalCombiningClass
        expect "Canonical combining class" $
          xmlCCC =? UCD.canonicalCombiningClass cp
        xmlName <- name
        expect "Name" $ xmlName =? UCD.name cp
        testMayEnumerated "Age" "age" "unassigned" UCD.age
        testEnumerated "Script" "sc" UCD.script
        testMayEnumerated "Block" "blk" "NB" UCD.block
        xmlScriptExts <- scriptExts
        expect "Script extensions" $
          sort xmlScriptExts =? sort (UCD.scriptExtensions cp)
        let testBoolean testName attrName f = do
              attr <- requireAttr attrName
              b <- readBoolean attr
              expect testName $ b =? f cp
        traverse_
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
          , ("Noncharacter code point", "NChar", UCD.noncharacterCodePoint)
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
        testMayEnumerated
          "Hangul syllable type"
          "hst"
          "NA"
          UCD.hangulSyllableType
        let testCPProp testName attrName f = do
              attr <- requireAttr attrName
              expected <-
                if attr == "#"
                  then pure (fromEnum cp)
                  else pure $ readHex attr
              expect testName $ expected =? fromEnum (f cp)
        traverse_
          (\(tn, an, f) -> testCPProp tn an f)
          [ ("Simple lowercase mapping", "slc", UCD.simpleLowercaseMapping)
          , ("Simple uppercase mapping", "suc", UCD.simpleUppercaseMapping)
          , ("Simple titlecase mapping", "stc", UCD.simpleTitlecaseMapping)
          , ("Simple case folding", "scf", UCD.simpleCaseFolding)
          ]
        let testCaseMappingProp testName attrName f = do
              attr <- requireAttr attrName
              let expected =
                    if attr == "#"
                      then [fromEnum cp]
                      else map readHex (T.words attr)
                  actual =
                    case f cp of
                      UCD.SingleCM c1 -> [fromEnum c1]
                      UCD.DoubleCM c1 c2 -> [fromEnum c1, fromEnum c2]
                      UCD.TripleCM c1 c2 c3 ->
                        [fromEnum c1, fromEnum c2, fromEnum c3]
              expect testName $ expected =? actual
        traverse_
          (\(tn, an, f) -> testCaseMappingProp tn an f)
          [ ("Lowercase mapping", "lc", UCD.lowercaseMapping)
          , ("Uppercase mapping", "uc", UCD.uppercaseMapping)
          , ("Titlecase mapping", "tc", UCD.titlecaseMapping)
          , ("Case folding", "cf", UCD.caseFolding)
          ]
        case getAttr "nt" of
          Nothing -> criticalFailure "Can't locate numeric type"
          Just ntStr ->
            case getAttr "nv" of
              Nothing -> criticalFailure "Can't locate numeric value"
              Just nvStr ->
                let ucdNum = UCD.numeric cp
                 in case ntStr of
                      "None" -> expect "Not numeric" $ Nothing =? ucdNum
                      "De" ->
                        case ucdNum of
                          Just (UCD.Decimal n) ->
                            expect "Numeric decimal" $
                            read (T.unpack nvStr) =? toInteger n
                          _ -> failure $ "Expected decimal, got " ++ show ucdNum
                      "Di" ->
                        case ucdNum of
                          Just (UCD.Digit n) ->
                            expect "Numeric digit" $
                            read (T.unpack nvStr) =? toInteger n
                          _ -> failure $ "Expected digit, got " ++ show ucdNum
                      "Nu" ->
                        case ucdNum of
                          Just (UCD.Numeric r) ->
                            let (numStr, mdenomStr) =
                                  break (== '/') $ T.unpack nvStr
                                numer = read numStr
                                denom =
                                  case mdenomStr of
                                    "" -> 1
                                    (_:denomStr) -> read denomStr
                                r' =
                                  toInteger (numerator r) %
                                  toInteger (denominator r)
                             in expect "Numeric numeric" $ (numer % denom) =? r'
                          _ -> failure $ "Expected numeric, got " ++ show ucdNum
                      _ ->
                        criticalFailure $
                        "Unrecognised numeric type: " ++ show ntStr
        testMayEnumerated "Decomposition type" "dt" "none" UCD.decompositionType
        let decompType = UCD.decompositionType cp
        decompMap <-
          case getAttr "dm" of
            Nothing -> criticalFailure "Can't find decomposition mapping"
            Just str -> pure $ map readHex $ T.words str
        case decompType of
          Just UCD.Canonical ->
            expect "Canonical decomposition" $
            foldMap (UCD.canonicalDecomposition . chr) decompMap =?
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
            foldMap (UCD.compatibilityDecomposition . chr) decompMap =?
            UCD.nontrivialCompatibilityDecomposition cp
        let getQuickCheck attr =
              case getAttr attr of
                Just "Y" -> pure $ Just True
                Just "N" -> pure $ Just False
                Just "M" -> pure Nothing
                _ -> criticalFailure "Can't parse quick check attribute"
        nfdQC <- getQuickCheck "NFD_QC"
        expect "NFD quick check" $ nfdQC =? Just (UCD.nfdQuickCheck cp)
        nfcQC <- getQuickCheck "NFC_QC"
        expect "NFC quick check" $ nfcQC =? UCD.nfcQuickCheck cp
        nfkdQC <- getQuickCheck "NFKD_QC"
        expect "NFKD quick check" $ nfkdQC =? Just (UCD.nfkdQuickCheck cp)
        nfkcQC <- getQuickCheck "NFKC_QC"
        expect "NFKC quick check" $ nfkcQC =? UCD.nfkcQuickCheck cp
          ------
        nfkcCF <-
          case getAttr "NFKC_CF" of
            Just "#" -> pure [cp]
            Just str -> pure $ map (toEnum . readHex) $ T.words str
            Nothing -> criticalFailure "Can't find NFKC_Casefold"
        case UCD.nfkcCaseFold cp of
          UCD.ShortCF c -> expect "NFKC_CF" $ nfkcCF =? [c]
          UCD.LongCF cs -> do
            expect "LongCF for long strings" $ 1 /=? length cs
            expect "NFKC_CF" $ nfkcCF =? cs
          -----
        testEnumerated "Joining type" "jt" UCD.joiningType
        testMayEnumerated
          "Joining group"
          "jg"
          "No_Joining_Group"
          UCD.joiningGroup
        testEnumerated "Vertical orientation" "vo" UCD.verticalOrientation
        testEnumerated "Line break" "lb" UCD.lineBreak
        testEnumerated "Grapheme cluster break" "GCB" UCD.graphemeClusterBreak
        testEnumerated "Sentence break" "SB" UCD.sentenceBreak
        testEnumerated "Word break" "WB" UCD.wordBreak
        testEnumerated "East Asian width" "ea" UCD.eastAsianWidth
        testEnumerated "Bidi class" "bc" UCD.bidiClass
          -------
        bmg <-
          case getAttr "bmg" of
            Just "" -> pure Nothing
            Just str -> pure $ Just $ toEnum $ readHex str
            Nothing -> criticalFailure "Can't locate bidi mirroring glyph"
        expect "Bidi mirroring glyph" $ bmg =? UCD.bidiMirroringGlyph cp
          -------
        testMayEnumerated
          "Bidi Paired Bracket Type"
          "bpt"
          "n"
          UCD.bidiPairedBracketType
        bpbText <- requireAttr "bpb"
        let bpbVal =
              case bpbText of
                "#" -> cp
                _ -> toEnum $ readHex bpbText
        expect "Bidi Paired Bracket" $ bpbVal =? UCD.bidiPairedBracket cp
    ]
  where
    canonicalCombiningClass =
      case getAttr "ccc" of
        Just cccStr -> liftIO $ readIO $ T.unpack cccStr
        Nothing -> criticalFailure "Can't locate canonical combining class"
    name =
      case getAttr "na" of
        Just rawName -> pure $ TE.encodeUtf8 $ T.replace "#" hexStr rawName
        Nothing -> criticalFailure "Can't locate name"
    hexStr =
      T.pack $
      let raw = map toUpper $ showHex (fromEnum cp) ""
       in if length raw < 4
            then replicate (4 - length raw) ' ' ++ raw
            else raw
    scriptExts =
      case getAttr "scx" of
        Nothing -> criticalFailure "Can't locate script extensions"
        Just scxStr ->
          traverse (parseEnumerated "script extension") $ T.words scxStr
    testEnumerated ::
         (Show p, Eq p, UCD.EnumeratedProperty p)
      => String
      -> Name
      -> (UCD.CodePoint -> p)
      -> Test ()
    testEnumerated testName attrName f = do
      attr <- requireAttr attrName
      xmlVal <- parseEnumerated testName attr
      expect testName $ xmlVal =? f cp
    testMayEnumerated ::
         (Show p, Eq p, UCD.EnumeratedProperty p)
      => String
      -> Name
      -> T.Text
      -> (UCD.CodePoint -> Maybe p)
      -> Test ()
    testMayEnumerated testName attrName absent f = do
      attr <- requireAttr attrName
      if attr == absent
        then expect (show attrName) $ Nothing =? f cp
        else do
          xmlVal <- parseEnumerated testName attr
          expect testName $ Just xmlVal =? f cp
    requireAttr :: Name -> Test T.Text
    requireAttr attrName =
      case getAttr attrName of
        Nothing -> criticalFailure $ "Can't locate attribute " ++ show attrName
        Just txt -> pure txt
    parseEnumerated :: UCD.EnumeratedProperty p => String -> T.Text -> Test p
    parseEnumerated propertyName txt =
      case find
             ((== txt8) . toUpper8 . UCD.abbreviatedPropertyValueName)
             [minBound .. maxBound] of
        Just p -> pure p
        Nothing ->
          criticalFailure $ "Can't parse " ++ propertyName ++ ": " ++ show txt
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
        Just str -> pure $ readHex str
        Nothing -> criticalFailure "Can't locate block start"
    blockEnd =
      case M.lookup "last-cp" $ elementAttributes blockDescr of
        Just str -> pure $ readHex str
        Nothing -> criticalFailure "Can't locate block end"
    block
      | blockName == "No_Block" = Just Nothing
      | otherwise =
        Just <$> flip find [minBound .. maxBound] $ \b ->
          let prepare =
                T.toLower . T.filter (\c -> c /= ' ' && c /= '-' && c /= '\'')
              bname = prepare $ T.dropEnd 5 $ T.pack $ show b
           in bname == prepare blockName

readHex :: T.Text -> Int
readHex t =
  case TR.hexadecimal t of
    Left err -> error err
    Right (n, "") -> n
    Right (_, rest) -> error $ "Leftover parse input " ++ show (T.unpack rest)

readBoolean :: T.Text -> Test Bool
readBoolean "Y" = pure True
readBoolean "N" = pure False
readBoolean txt = criticalFailure $ "Unable to read boolean: " ++ show txt

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
