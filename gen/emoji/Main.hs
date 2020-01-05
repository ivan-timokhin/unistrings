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

module Main
  ( main
  ) where

import System.Directory (createDirectoryIfMissing)

import Driver
  ( Locate(Locate, cSymbolName, moduleName, modulePath, path)
  , bool
  , generateTestsAt
  , processTableAtAs
  )
import qualified Parser as P
import qualified Runner as R

import Emoji
  ( Props(emoji, emojiComponent, emojiModifier, emojiModifierBase,
      emojiPresentation, extendedPictographic)
  , fetch
  )

main :: IO ()
main = do
  props <- fetch
  createDirectoryIfMissing True "unistring-emoji-data/generated/cbits"
  createDirectoryIfMissing
    True
    "unistring-emoji-data/generated/hs/Data/Unistring/Emoji/Data/Internal"
  createDirectoryIfMissing True "unistring-emoji-data/generated/test_data"
  let fullPartitionings = (4, 16)
      tableToVector = P.tableToVector False
      processTable name f =
        processTableAtAs emojiLocate bool fullPartitionings name $
        tableToVector $ f props
  R.traverse_
    id
    [ processTable "emoji" emoji
    , processTable "emoji_presentation" emojiPresentation
    , generateTestsAt emojiLocate "emoji_modifier" $
      tableToVector $ emojiModifier props
    , processTable "emoji_modifier_base" emojiModifierBase
    , processTable "emoji_component" emojiComponent
    , processTable "extended_pictographic" extendedPictographic
    ]

emojiLocate :: Locate
emojiLocate =
  Locate
    { path = ("unistring-emoji-data/generated/" <>)
    , moduleName = ("Data.Unistring.Emoji.Data.Internal." <>)
    , modulePath = ("Data/Unistring/Emoji/Data/Internal/" <>)
    , cSymbolName = ("unistring_emoji_data__" <>)
    }
