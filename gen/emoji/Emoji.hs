{-
Copyright 2020 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Emoji where

import qualified Data.Map.Strict as Map

import Parser (Table, fetchBinaryMulti)

data Props =
  Props
    { emoji :: Table () () Bool
    , emojiPresentation :: Table () () Bool
    , emojiModifier :: Table () () Bool
    , emojiModifierBase :: Table () () Bool
    , emojiComponent :: Table () () Bool
    , extendedPictographic :: Table () () Bool
    }
  deriving (Show)

fetch :: IO Props
fetch = do
  m <- fetchBinaryMulti "data/ucd/emoji/emoji-data.txt"
  let get prop =
        case Map.lookup prop m of
          Nothing -> fail $ "Can't find property " ++ show prop
          Just tbl -> pure tbl
  emoji <- get "Emoji"
  emojiPresentation <- get "Emoji_Presentation"
  emojiModifier <- get "Emoji_Modifier"
  emojiModifierBase <- get "Emoji_Modifier_Base"
  emojiComponent <- get "Emoji_Component"
  extendedPictographic <- get "Extended_Pictographic"
  pure $ Props {..}
