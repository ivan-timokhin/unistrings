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

import qualified Data.ByteString.Char8 as B8
import Data.Foldable (for_)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Data.Unistring.Emoji.Data as Emoji
import Data.Unistring.UCD (CodePoint)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Properties"
    [ testProperty "Emoji" "emoji" Emoji.emoji
    , testProperty
        "Emoji presentation"
        "emoji_presentation"
        Emoji.emojiPresentation
    , testProperty "Emoji modifier" "emoji_modifier" Emoji.emojiModifier
    , testProperty
        "Emoji modifier base"
        "emoji_modifier_base"
        Emoji.emojiModifierBase
    , testProperty "Emoji component" "emoji_component" Emoji.emojiComponent
    , testProperty
        "Extended pictographic"
        "extended_pictographic"
        Emoji.extendedPictographic
    ]

testProperty :: String -> FilePath -> (CodePoint -> Bool) -> TestTree
testProperty name path prop =
  testCase name $ do
    testData <-
      B8.lines <$> B8.readFile ("generated/test_data/" ++ path ++ ".txt")
    for_ (zip testData [minBound .. maxBound]) $ \(refStr, cp) ->
      assertEqual (show cp) refStr $ B8.pack $ show $ prop cp
