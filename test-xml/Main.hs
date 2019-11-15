{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Archive.Zip (mkEntrySelector, sourceEntry, withArchive)
import Control.Applicative ((<|>), liftA2)
import Control.Monad (when)
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
import System.Exit (exitFailure)
import Test.HUnit
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
        TestList
          [ TestLabel "groups" $
            TestList $ groupsOnly groups : zipWith testGroup [0 ..] groups
          , TestLabel "blocks" $ TestList $ map testBlock blocks
          ]
  results <- runTestTT tests
  when (errors results + failures results /= 0) exitFailure

groupsOnly :: [Element] -> Test
groupsOnly els =
  TestLabel "Only groups at top level" $
  TestCase $ for_ els $ \el -> nameLocalName (elementName el) @?= "group"

testGroup :: Int -> Element -> Test
testGroup n group =
  TestLabel ("Group #" ++ show n) $ TestList [validRecordTypes, cpTests]
  where
    validRecordTypes =
      TestLabel "Valid record types" $
      TestList $
      flip map (elementChildren group) $ \el ->
        TestCase $
        nameLocalName (elementName el) `elem`
        ["reserved", "noncharacter", "surrogate", "char"] @?
        "Unrecognized record type"
    cpTests =
      TestLabel "Code point properties" $
      TestList $
      flip map (elementChildren group) $ \el ->
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
                 in TestList $ map testCP' [toEnum start .. toEnum end]
              Nothing ->
                case M.lookup "cp" (elementAttributes el) of
                  Just cpStr ->
                    let cp = readHex cpStr
                     in testCP' $ toEnum cp
                  Nothing -> error $ "Unrecognized record type: " ++ show el
    attrs = elementAttributes group

testCP :: [Element] -> (Name -> Maybe T.Text) -> UCD.CodePoint -> Test
testCP children getAttr cp =
  TestLabel (show cp) $
  TestList
    [ "Name aliases" ~: testCPAliases children cp
    , "Properties" ~:
      (do testEnumerated "General category" "gc" UCD.generalCategory
          xmlCCC <- canonicalCombiningClass
          assertEqual "Canonical combining class" xmlCCC $
            UCD.canonicalCombiningClass cp
          xmlName <- name
          assertEqual "Name" xmlName $ UCD.name cp
          testMayEnumerated "Age" "age" "unassigned" UCD.age
          testEnumerated "Script" "sc" UCD.script
          testMayEnumerated "Block" "blk" "NB" UCD.block
          xmlScriptExts <- scriptExts
          assertEqual "Script extensions" (sort xmlScriptExts) $
            sort $ UCD.scriptExtensions cp
          let testBoolean testName attrName f = do
                attr <- requireAttr attrName
                b <- readBoolean attr
                assertEqual testName b (f cp)
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
                assertEqual testName expected $ fromEnum $ f cp
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
                assertEqual testName expected actual
          traverse_
            (\(tn, an, f) -> testCaseMappingProp tn an f)
            [ ("Lowercase mapping", "lc", UCD.lowercaseMapping)
            , ("Uppercase mapping", "uc", UCD.uppercaseMapping)
            , ("Titlecase mapping", "tc", UCD.titlecaseMapping)
            , ("Case folding", "cf", UCD.caseFolding)
            ]
          case getAttr "nt" of
            Nothing -> assertFailure "Can't locate numeric type"
            Just ntStr ->
              case getAttr "nv" of
                Nothing -> assertFailure "Can't locate numeric value"
                Just nvStr ->
                  let ucdNum = UCD.numeric cp
                   in case ntStr of
                        "None" -> assertEqual "Not numeric" Nothing ucdNum
                        "De" ->
                          case ucdNum of
                            Just (UCD.Decimal n) ->
                              assertEqual
                                "Numeric decimal"
                                (read $ T.unpack nvStr) $
                              toInteger n
                            _ ->
                              assertFailure $
                              "Expected decimal, got " ++ show ucdNum
                        "Di" ->
                          case ucdNum of
                            Just (UCD.Digit n) ->
                              assertEqual
                                "Numeric digit"
                                (read $ T.unpack nvStr) $
                              toInteger n
                            _ ->
                              assertFailure $
                              "Expected digit, got " ++ show ucdNum
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
                               in assertEqual
                                    "Numeric numeric"
                                    (numer % denom)
                                    r'
                            _ ->
                              assertFailure $
                              "Expected numeric, got " ++ show ucdNum
                        _ ->
                          assertFailure $
                          "Unrecognised numeric type: " ++ show ntStr
          testMayEnumerated
            "Decomposition type"
            "dt"
            "none"
            UCD.decompositionType
          let decompType = UCD.decompositionType cp
          decompMap <-
            case getAttr "dm" of
              Nothing -> assertFailure "Can't find decomposition mapping"
              Just str -> pure $ map readHex $ T.words str
          case decompType of
            Just UCD.Canonical ->
              assertEqual
                "Canonical decomposition"
                (foldMap (UCD.canonicalDecomposition . chr) decompMap) $
              UCD.nontrivialCanonicalDecomposition cp
            _ ->
              assertEqual "Canonical decomposition" [] $
              UCD.nontrivialCanonicalDecomposition cp
          case decompType of
            Nothing ->
              assertEqual "Compatibility decomposition" [] $
              UCD.nontrivialCompatibilityDecomposition cp
            _ ->
              assertEqual
                "Compatibility decomposition"
                (foldMap (UCD.compatibilityDecomposition . chr) decompMap) $
              UCD.nontrivialCompatibilityDecomposition cp
          let getQuickCheck attr =
                case getAttr attr of
                  Just "Y" -> pure $ Just True
                  Just "N" -> pure $ Just False
                  Just "M" -> pure Nothing
                  _ -> assertFailure "Can't parse quick check attribute"
          nfdQC <- getQuickCheck "NFD_QC"
          assertEqual "NFD quick check" nfdQC $ Just $ UCD.nfdQuickCheck cp
          nfcQC <- getQuickCheck "NFC_QC"
          assertEqual "NFC quick check" nfcQC $ UCD.nfcQuickCheck cp
          nfkdQC <- getQuickCheck "NFKD_QC"
          assertEqual "NFKD quick check" nfkdQC $ Just $ UCD.nfkdQuickCheck cp
          nfkcQC <- getQuickCheck "NFKC_QC"
          assertEqual "NFKC quick check" nfkcQC $ UCD.nfkcQuickCheck cp
          ------
          nfkcCF <-
            case getAttr "NFKC_CF" of
              Just "#" -> pure [cp]
              Just str -> pure $ map (toEnum . readHex) $ T.words str
              Nothing -> assertFailure "Can't find NFKC_Casefold"
          case UCD.nfkcCaseFold cp of
            UCD.ShortCF c -> assertEqual "NFKC_CF" nfkcCF [c]
            UCD.LongCF cs -> do
              assert $ length cs /= 1
              assertEqual "NFKC_CF" nfkcCF cs
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
          testEnumerated "Word break" "WB" UCD.wordBreak)
    ]
  where
    canonicalCombiningClass =
      case getAttr "ccc" of
        Just cccStr -> readIO $ T.unpack cccStr
        Nothing -> assertFailure "Can't locate canonical combining class"
    name =
      case getAttr "na" of
        Just rawName -> pure $ TE.encodeUtf8 $ T.replace "#" hexStr rawName
        Nothing -> assertFailure "Can't locate name"
    hexStr =
      T.pack $
      let raw = map toUpper $ showHex (fromEnum cp) ""
       in if length raw < 4
            then replicate (4 - length raw) ' ' ++ raw
            else raw
    scriptExts =
      case getAttr "scx" of
        Nothing -> assertFailure "Can't locate script extensions"
        Just scxStr ->
          traverse (parseEnumerated "script extension") $ T.words scxStr
    testEnumerated ::
         (Show p, Eq p, UCD.EnumeratedProperty p)
      => String
      -> Name
      -> (UCD.CodePoint -> p)
      -> IO ()
    testEnumerated testName attrName f = do
      attr <- requireAttr attrName
      xmlVal <- parseEnumerated testName attr
      assertEqual testName xmlVal $ f cp
    testMayEnumerated ::
         (Show p, Eq p, UCD.EnumeratedProperty p)
      => String
      -> Name
      -> T.Text
      -> (UCD.CodePoint -> Maybe p)
      -> IO ()
    testMayEnumerated testName attrName absent f = do
      attr <- requireAttr attrName
      if attr == absent
        then assertEqual (show attrName) Nothing $ f cp
        else do
          xmlVal <- parseEnumerated testName attr
          assertEqual testName (Just xmlVal) $ f cp
    requireAttr :: Name -> IO T.Text
    requireAttr attrName =
      case getAttr attrName of
        Nothing -> assertFailure $ "Can't locate attribute " ++ show attrName
        Just txt -> pure txt
    parseEnumerated :: UCD.EnumeratedProperty p => String -> T.Text -> IO p
    parseEnumerated propertyName txt =
      case find
             ((== txt8) . toUpper8 . UCD.abbreviatedPropertyValueName)
             [minBound .. maxBound] of
        Just p -> pure p
        Nothing ->
          assertFailure $ "Can't parse " ++ propertyName ++ ": " ++ show txt
      where
        txt8 = toUpper8 $ TE.encodeUtf8 txt
    toUpper8 = B.map (.|. 0x20)

testCPAliases :: [Element] -> UCD.CodePoint -> IO ()
testCPAliases children cp = do
  aliases <-
    sort <$> traverse (\elt -> liftA2 (,) (aliasType elt) (alias elt)) aliasElts
  assertEqual "Name aliases" aliases $ UCD.nameAliases cp
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
          assertFailure $ "Unrecognised name alias type" ++ show otherStr
        Nothing -> assertFailure "Can't locate alias type"
    alias elt =
      case M.lookup "alias" (elementAttributes elt) of
        Just str -> pure $ TE.encodeUtf8 str
        Nothing -> assertFailure "Can't locate alias"

testBlock :: Element -> Test
testBlock blockDescr =
  TestLabel (T.unpack blockName) $
  TestCase $ do
    start <- (toEnum :: Int -> UCD.CodePoint) <$> blockStart
    end <- toEnum <$> blockEnd
    for_ [start .. end] $ \cp ->
      assertEqual (show cp) block $ Just $ UCD.block cp
  where
    blockName = fromJust $ M.lookup "name" $ elementAttributes blockDescr
    blockStart =
      case M.lookup "first-cp" $ elementAttributes blockDescr of
        Just str -> pure $ readHex str
        Nothing -> assertFailure "Can't locate block start"
    blockEnd =
      case M.lookup "last-cp" $ elementAttributes blockDescr of
        Just str -> pure $ readHex str
        Nothing -> assertFailure "Can't locate block end"
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

readBoolean :: T.Text -> IO Bool
readBoolean "Y" = pure True
readBoolean "N" = pure False
readBoolean txt = assertFailure $ "Unable to read boolean: " ++ show txt

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
