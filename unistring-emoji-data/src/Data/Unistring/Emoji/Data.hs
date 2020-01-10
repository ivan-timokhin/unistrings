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
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Emoji.Data
Description : Emoji-related code point properties
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental

A number of emoji-related code point properties defined
in [UTS #51 ‘Unicode Emoji’](https://www.unicode.org/reports/tr51/).
For detailed description of these properties, see the UTS.
-}
module Data.Unistring.Emoji.Data
  ( emoji
  , emojiPresentation
  , emojiModifier
  , emojiModifierBase
  , emojiComponent
  , extendedPictographic
  ) where

import Data.Unistring.UCD (IsCodePoint(toCodePoint))

import qualified Data.Unistring.Emoji.Data.Internal.Emoji as E
import qualified Data.Unistring.Emoji.Data.Internal.EmojiComponent as EC
import qualified Data.Unistring.Emoji.Data.Internal.EmojiModifierBase as EMB
import qualified Data.Unistring.Emoji.Data.Internal.EmojiPresentation as EmP
import qualified Data.Unistring.Emoji.Data.Internal.ExtendedPictographic as ExP

-- | Code points representing emoji characters
--
-- === __Examples__
--
-- Things you'd normally think of as emoji are included.
--
-- >>> import Data.Unistring.UCD (name)
-- >>> name '\x2614'
-- "UMBRELLA WITH RAIN DROPS"
-- >>> emoji '\x2614'
-- True
--
-- But also included are some other code points that can be rendered
-- as emoji with the help of variation selectors (at least, per the
-- Unicode specification; actual font/renderer support is not
-- guaranteed)
--
-- >>> emoji '0'
-- True
emoji :: IsCodePoint cp => cp -> Bool
emoji = withCP E.retrieve

-- | Code points that are rendered as emoji by default, without the
-- use of variation selectors.
--
-- === __Examples__
--
-- All 'emojiPresentation' code points are 'emoji'
--
-- >>> all emoji $ filter emojiPresentation ['\x0'..]
-- True
--
-- But not all 'emoji' code points are rendered as emoji by default
--
-- >>> emoji '0'
-- True
-- >>> emojiPresentation '0'
-- False
emojiPresentation :: IsCodePoint cp => cp -> Bool
emojiPresentation = withCP EmP.retrieve

-- | Code points used to modify the appearance of the preceding emoji
--
-- === __Examples__
--
-- Currently, the only available modifiers are for changing the
-- displayed skin colour on a [Fitzpatrick scale](https://en.wikipedia.org/wiki/Fitzpatrick_scale).
--
-- >>> import Data.Unistring.UCD (toCodePoint, name)
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter emojiModifier ['\0'..]
-- (toEnum 0x1f3fb,"EMOJI MODIFIER FITZPATRICK TYPE-1-2")
-- (toEnum 0x1f3fc,"EMOJI MODIFIER FITZPATRICK TYPE-3")
-- (toEnum 0x1f3fd,"EMOJI MODIFIER FITZPATRICK TYPE-4")
-- (toEnum 0x1f3fe,"EMOJI MODIFIER FITZPATRICK TYPE-5")
-- (toEnum 0x1f3ff,"EMOJI MODIFIER FITZPATRICK TYPE-6")
emojiModifier :: IsCodePoint cp => cp -> Bool
emojiModifier = withCP $ \cp -> 0x1f3fb <= cp && cp <= 0x1f3ff

-- | Code points, the rendering of which can be modified via
-- 'emojiModifier'.
--
-- === __Examples__
--
-- Since 'emojiModifier' determine skin colour, this category includes
-- various emoji depicting people or parts thereof.
--
-- >>> import Data.Unistring.UCD (toCodePoint, name)
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ take 20 $ filter emojiModifierBase ['\0'..]
-- (toEnum 0x261d,"WHITE UP POINTING INDEX")
-- (toEnum 0x26f9,"PERSON WITH BALL")
-- (toEnum 0x270a,"RAISED FIST")
-- (toEnum 0x270b,"RAISED HAND")
-- (toEnum 0x270c,"VICTORY HAND")
-- (toEnum 0x270d,"WRITING HAND")
-- (toEnum 0x1f385,"FATHER CHRISTMAS")
-- (toEnum 0x1f3c2,"SNOWBOARDER")
-- (toEnum 0x1f3c3,"RUNNER")
-- (toEnum 0x1f3c4,"SURFER")
-- (toEnum 0x1f3c7,"HORSE RACING")
-- (toEnum 0x1f3ca,"SWIMMER")
-- (toEnum 0x1f3cb,"WEIGHT LIFTER")
-- (toEnum 0x1f3cc,"GOLFER")
-- (toEnum 0x1f442,"EAR")
-- (toEnum 0x1f443,"NOSE")
-- (toEnum 0x1f446,"WHITE UP POINTING BACKHAND INDEX")
-- (toEnum 0x1f447,"WHITE DOWN POINTING BACKHAND INDEX")
-- (toEnum 0x1f448,"WHITE LEFT POINTING BACKHAND INDEX")
-- (toEnum 0x1f449,"WHITE RIGHT POINTING BACKHAND INDEX")
--
-- (U+1F3C7 ‘HORSE RACING’ actually depicts a man sitting on a horse).
emojiModifierBase :: IsCodePoint cp => cp -> Bool
emojiModifierBase = withCP EMB.retrieve

-- | Code points that do not typically appear on emoji keyboards as
-- separate choices, but can nevertheless participate in emoji
-- sequences.
--
-- === __Examples__
--
-- Some emoji components are also emoji
--
-- >>> emoji '0'
-- True
-- >>> emojiComponent '0'
-- True
--
-- But some aren't
--
-- >>> import Data.Unistring.UCD (name)
-- >>> name '\x200D'
-- "ZERO WIDTH JOINER"
-- >>> emoji '\x200D'
-- False
-- >>> emojiComponent '\x200D'
-- True
emojiComponent :: IsCodePoint cp => cp -> Bool
emojiComponent = withCP EC.retrieve

-- | Code points for emoji and similar pictographs.  This property
-- main use is in segmentation; see [UAX #29](https://www.unicode.org/reports/tr29/)
-- for details.
--
-- === __Examples__
--
-- Most emoji are in this category
--
-- >>> emoji '\x2614'
-- True
-- >>> extendedPictographic '\x2614'
-- True
--
-- but some 'emojiComponent' aren't
--
-- >>> emoji '0'
-- True
-- >>> extendedPictographic '0'
-- False
--
-- In addition, this category includes some reserved code points for
-- future-proofing segmentation
--
-- >>> import Data.Unistring.UCD (age)
-- >>> age '\x1fa7b'
-- Nothing
-- >>> extendedPictographic '\x1fa7b'
-- True
extendedPictographic :: IsCodePoint cp => cp -> Bool
extendedPictographic = withCP ExP.retrieve

withCP :: IsCodePoint cp => (Int -> a) -> cp -> a
withCP f = f . fromEnum . toCodePoint
