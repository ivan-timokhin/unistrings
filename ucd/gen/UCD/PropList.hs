{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module UCD.PropList where

import qualified Data.Map.Strict as Map

import UCD.Common (Table, fetchBinaryMulti)

data Props =
  Props
    { whiteSpace :: Table () () Bool
    , bidiControl :: Table () () Bool
    , joinControl :: Table () () Bool
    , dash :: Table () () Bool
    , quotationMark :: Table () () Bool
    , terminalPunctuation :: Table () () Bool
    , hexDigit :: Table () () Bool
    , asciiHexDigit :: Table () () Bool
    , ideographic :: Table () () Bool
    , diacritic :: Table () () Bool
    , extender :: Table () () Bool
    , noncharacterCodePoint :: Table () () Bool
    , idsBinaryOperator :: Table () () Bool
    , idsTrinaryOperator :: Table () () Bool
    , radical :: Table () () Bool
    , unifiedIdeograph :: Table () () Bool
    , deprecated :: Table () () Bool
    , softDotted :: Table () () Bool
    , logicalOrderException :: Table () () Bool
    , sentenceTerminal :: Table () () Bool
    , variationSelector :: Table () () Bool
    , patternWhiteSpace :: Table () () Bool
    , patternSyntax :: Table () () Bool
    , prependedConcatenationMark :: Table () () Bool
    , regionalIndicator :: Table () () Bool
    }
  deriving (Show)

fetch :: IO Props
fetch = do
  m <- fetchBinaryMulti "PropList.txt"
  let get prop =
        case Map.lookup prop m of
          Nothing -> fail $ "Can't find property " ++ show prop
          Just tbl -> pure tbl
  whiteSpace <- get "White_Space"
  bidiControl <- get "Bidi_Control"
  joinControl <- get "Join_Control"
  dash <- get "Dash"
  quotationMark <- get "Quotation_Mark"
  terminalPunctuation <- get "Terminal_Punctuation"
  hexDigit <- get "Hex_Digit"
  asciiHexDigit <- get "ASCII_Hex_Digit"
  ideographic <- get "Ideographic"
  diacritic <- get "Diacritic"
  extender <- get "Extender"
  noncharacterCodePoint <- get "Noncharacter_Code_Point"
  idsBinaryOperator <- get "IDS_Binary_Operator"
  idsTrinaryOperator <- get "IDS_Trinary_Operator"
  radical <- get "Radical"
  unifiedIdeograph <- get "Unified_Ideograph"
  deprecated <- get "Deprecated"
  softDotted <- get "Soft_Dotted"
  logicalOrderException <- get "Logical_Order_Exception"
  sentenceTerminal <- get "Sentence_Terminal"
  variationSelector <- get "Variation_Selector"
  patternWhiteSpace <- get "Pattern_White_Space"
  patternSyntax <- get "Pattern_Syntax"
  prependedConcatenationMark <- get "Prepended_Concatenation_Mark"
  regionalIndicator <- get "Regional_Indicator"
  pure $ Props {..}
