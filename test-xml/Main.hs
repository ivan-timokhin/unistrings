{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Archive.Zip (mkEntrySelector, sourceEntry, withArchive)
import Control.Applicative ((<|>), liftA2)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.Foldable (for_, traverse_)
import Data.List (find, sort)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Traversable (for)
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
  TestLabel ("Group #" ++ show n) $
  TestCase $ do
    let attrs = elementAttributes group
    for_ (elementChildren group) $ \el -> do
      nameLocalName (elementName el) `elem`
        ["reserved", "noncharacter", "surrogate", "char"] @?
        "Unrecognized record type"
      let testCP' =
            testCP $ \nm ->
              M.lookup nm (elementAttributes el) <|> M.lookup nm attrs
      case M.lookup "first-cp" (elementAttributes el) of
        Just startStr -> do
          start <- readHex startStr
          end <-
            readHex =<<
            maybe
              (assertFailure "Missing \"last-cp\" attribute")
              pure
              (M.lookup "last-cp" (elementAttributes el))
          for_ [toEnum start .. toEnum end] testCP'
        Nothing ->
          case M.lookup "cp" (elementAttributes el) of
            Just cpStr -> do
              cp <- readHex cpStr
              testCP' $ toEnum cp
              testCPAliases (elementChildren el) $ toEnum cp
            Nothing -> assertFailure $ "Unrecognized record type: " ++ show el

testCP :: (Name -> Maybe T.Text) -> UCD.CodePoint -> IO ()
testCP getAttr cp = do
  xmlGC <- generalCategory
  assertEqual "General category" xmlGC $ UCD.generalCategory cp
  xmlCCC <- canonicalCombiningClass
  assertEqual "Canonical combining class" xmlCCC $
    UCD.canonicalCombiningClass cp
  xmlName <- name
  assertEqual "Name" xmlName $ UCD.name cp
  xmlAge <- age
  assertEqual "Age" xmlAge $ UCD.age cp
  xmlScript <- script
  assertEqual "Script" xmlScript $ UCD.script cp
  xmlBlock <- block
  assertEqual "Block" xmlBlock $ UCD.block cp
  xmlScriptExts <- scriptExts
  assertEqual "Script extensions" (sort xmlScriptExts) $
    sort $ UCD.scriptExtensions cp
  let testBoolean testName attrName f =
        case getAttr attrName of
          Nothing -> assertFailure $ "Can't locate " ++ show testName
          Just attr -> do
            b <- readBoolean attr
            assertEqual testName b (f cp)
  traverse_
    (\(tn, an, f) -> testBoolean tn an f)
    [ ("White space", "WSpace", UCD.whiteSpace)
    , ("Bidi control", "Bidi_C", UCD.bidiControl)
    , ("Join control", "Join_C", UCD.joinControl)
    , ("Dash", "Dash", UCD.dash)
    , ("Hyphen", "Hyphen", UCD.hyphen)
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
    , ("Prepended concatenation mark", "PCM", UCD.prependedConcatenationMark)
    , ("Regional indicator", "RI", UCD.regionalIndicator)
    ]
  where
    generalCategory =
      case getAttr "gc" of
        Just gc ->
          case gc of
            "Lu" -> pure UCD.UppercaseLetter
            "Ll" -> pure UCD.LowercaseLetter
            "Lt" -> pure UCD.TitlecaseLetter
            "Lm" -> pure UCD.ModifierLetter
            "Lo" -> pure UCD.OtherLetter
            "Mn" -> pure UCD.NonSpacingMark
            "Mc" -> pure UCD.SpacingCombiningMark
            "Me" -> pure UCD.EnclosingMark
            "Nd" -> pure UCD.DecimalNumber
            "Nl" -> pure UCD.LetterNumber
            "No" -> pure UCD.OtherNumber
            "Pc" -> pure UCD.ConnectorPunctuation
            "Pd" -> pure UCD.DashPunctuation
            "Ps" -> pure UCD.OpenPunctuation
            "Pe" -> pure UCD.ClosePunctuation
            "Pi" -> pure UCD.InitialQuote
            "Pf" -> pure UCD.FinalQuote
            "Po" -> pure UCD.OtherPunctuation
            "Sm" -> pure UCD.MathSymbol
            "Sc" -> pure UCD.CurrencySymbol
            "Sk" -> pure UCD.ModifierSymbol
            "So" -> pure UCD.OtherSymbol
            "Zs" -> pure UCD.Space
            "Zl" -> pure UCD.LineSeparator
            "Zp" -> pure UCD.ParagraphSeparator
            "Cc" -> pure UCD.Control
            "Cf" -> pure UCD.Format
            "Cs" -> pure UCD.Surrogate
            "Co" -> pure UCD.PrivateUse
            "Cn" -> pure UCD.NotAssigned
            _ ->
              assertFailure $ "Unrecognised general category value: " ++ show gc
        Nothing -> assertFailure "Can't locate general category"
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
    age =
      case getAttr "age" of
        Nothing -> assertFailure "Can't locate age"
        Just "unassigned" -> pure Nothing
        Just rawAge ->
          let rawAge8 = TE.encodeUtf8 rawAge
           in case find
                     ((== rawAge8) . UCD.abbreviatedPropertyValueName)
                     [minBound .. maxBound] of
                Nothing -> assertFailure $ "Can't parse age: " ++ show rawAge
                Just a -> pure $ Just a
    script =
      case getAttr "sc" of
        Nothing -> assertFailure "Can't locate script"
        Just scStr ->
          case find
                 (\p ->
                    scStr == TE.decodeUtf8 (UCD.abbreviatedPropertyValueName p))
                 [minBound .. maxBound] of
            Nothing -> assertFailure $ "Can't parse script: " ++ show scStr
            Just sc -> pure sc
    block =
      case getAttr "blk" of
        Nothing -> assertFailure "Can't locate block"
        Just blkStr ->
          let blkStr8 = TE.encodeUtf8 blkStr
           in case find
                     ((== blkStr8) . UCD.abbreviatedPropertyValueName)
                     [minBound .. maxBound] of
                Nothing -> assertFailure $ "Can't parse block: " ++ show blkStr
                Just blk -> pure blk
    scriptExts =
      case getAttr "scx" of
        Nothing -> assertFailure "Can't locate script extensions"
        Just scxStr ->
          for (map TE.encodeUtf8 $ T.words scxStr) $ \bstr ->
            case find
                   ((== bstr) . UCD.abbreviatedPropertyValueName)
                   [minBound .. maxBound] of
              Nothing ->
                assertFailure $ "Can't parse script extension: " ++ show bstr
              Just sc -> pure sc

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
        Just str -> readHex str
        Nothing -> assertFailure "Can't locate block start"
    blockEnd =
      case M.lookup "last-cp" $ elementAttributes blockDescr of
        Just str -> readHex str
        Nothing -> assertFailure "Can't locate block end"
    block
      | blockName == "No_Block" = Just UCD.NoBlock
      | otherwise =
        flip find [succ minBound .. maxBound] $ \b ->
          let prepare =
                T.toLower . T.filter (\c -> c /= ' ' && c /= '-' && c /= '\'')
              bname = prepare $ T.dropEnd 5 $ T.pack $ show b
           in bname == prepare blockName

readHex :: T.Text -> IO Int
readHex t =
  case TR.hexadecimal t of
    Left err -> assertFailure err
    Right (n, rest) -> assertString (T.unpack rest) >> pure n

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
