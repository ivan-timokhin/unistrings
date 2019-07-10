{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Archive.Zip (mkEntrySelector, sourceEntry, withArchive)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (toUpper)
import Data.Foldable (for_)
import Data.List (find)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, mapMaybe)
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
  let tests = TestList $ groupsOnly groups : zipWith testGroup [0 ..] groups
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

readHex :: T.Text -> IO Int
readHex t =
  case TR.hexadecimal t of
    Left err -> assertFailure err
    Right (n, rest) -> assertString (T.unpack rest) >> pure n

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
