{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module UCD.Scripts where

import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

import UCD.Common (Table(Table), comment, comments, range, tableP)
import Data.UCD.Internal.Types (Script(..))

fetch :: IO (Table () () Script)
fetch = do
  txt <- B.readFile "data/latest/ucd/Scripts.txt"
  case A.parseOnly (parser <* A.endOfInput) txt of
    Left err -> fail err
    Right scripts -> pure scripts

parser :: A.Parser (Table () () Script)
parser = do
  comments
  fmap Table $
    many $ do
      rng <- range
      A.skipSpace
      scr <- script
      A.skipSpace
      comment
      _ <- A.char '\n'
      comments
      pure $ scr <$ rng

script :: A.Parser Script
script =
  tableP
    [ "Adlam" ~> AdlamScript
    , "Ahom" ~> AhomScript
    , "Anatolian_Hieroglyphs" ~> AnatolianHieroglyphsScript
    , "Arabic" ~> ArabicScript
    , "Armenian" ~> ArmenianScript
    , "Avestan" ~> AvestanScript
    , "Balinese" ~> BalineseScript
    , "Bamum" ~> BamumScript
    , "Bassa_Vah" ~> BassaVahScript
    , "Batak" ~> BatakScript
    , "Bengali" ~> BengaliScript
    , "Bhaiksuki" ~> BhaiksukiScript
    , "Bopomofo" ~> BopomofoScript
    , "Brahmi" ~> BrahmiScript
    , "Braille" ~> BrailleScript
    , "Buginese" ~> BugineseScript
    , "Buhid" ~> BuhidScript
    , "Canadian_Aboriginal" ~> CanadianAboriginalScript
    , "Carian" ~> CarianScript
    , "Caucasian_Albanian" ~> CaucasianAlbanianScript
    , "Chakma" ~> ChakmaScript
    , "Cham" ~> ChamScript
    , "Cherokee" ~> CherokeeScript
    , "Common" ~> CommonScript
    , "Coptic" ~> CopticScript
    , "Cuneiform" ~> CuneiformScript
    , "Cypriot" ~> CypriotScript
    , "Cyrillic" ~> CyrillicScript
    , "Deseret" ~> DeseretScript
    , "Devanagari" ~> DevanagariScript
    , "Dogra" ~> DograScript
    , "Duployan" ~> DuployanScript
    , "Egyptian_Hieroglyphs" ~> EgyptianHieroglyphsScript
    , "Elbasan" ~> ElbasanScript
    , "Elymaic" ~> ElymaicScript
    , "Ethiopic" ~> EthiopicScript
    , "Georgian" ~> GeorgianScript
    , "Glagolitic" ~> GlagoliticScript
    , "Gothic" ~> GothicScript
    , "Grantha" ~> GranthaScript
    , "Greek" ~> GreekScript
    , "Gujarati" ~> GujaratiScript
    , "Gunjala_Gondi" ~> GunjalaGondiScript
    , "Gurmukhi" ~> GurmukhiScript
    , "Han" ~> HanScript
    , "Hangul" ~> HangulScript
    , "Hanifi_Rohingya" ~> HanifiRohingyaScript
    , "Hanunoo" ~> HanunooScript
    , "Hatran" ~> HatranScript
    , "Hebrew" ~> HebrewScript
    , "Hiragana" ~> HiraganaScript
    , "Imperial_Aramaic" ~> ImperialAramaicScript
    , "Inherited" ~> InheritedScript
    , "Inscriptional_Pahlavi" ~> InscriptionalPahlaviScript
    , "Inscriptional_Parthian" ~> InscriptionalParthianScript
    , "Javanese" ~> JavaneseScript
    , "Kaithi" ~> KaithiScript
    , "Kannada" ~> KannadaScript
    , "Katakana" ~> KatakanaScript
    , "Kayah_Li" ~> KayahLiScript
    , "Kharoshthi" ~> KharoshthiScript
    , "Khmer" ~> KhmerScript
    , "Khojki" ~> KhojkiScript
    , "Khudawadi" ~> KhudawadiScript
    , "Lao" ~> LaoScript
    , "Latin" ~> LatinScript
    , "Lepcha" ~> LepchaScript
    , "Limbu" ~> LimbuScript
    , "Linear_A" ~> LinearAScript
    , "Linear_B" ~> LinearBScript
    , "Lisu" ~> LisuScript
    , "Lycian" ~> LycianScript
    , "Lydian" ~> LydianScript
    , "Mahajani" ~> MahajaniScript
    , "Makasar" ~> MakasarScript
    , "Malayalam" ~> MalayalamScript
    , "Mandaic" ~> MandaicScript
    , "Manichaean" ~> ManichaeanScript
    , "Marchen" ~> MarchenScript
    , "Masaram_Gondi" ~> MasaramGondiScript
    , "Medefaidrin" ~> MedefaidrinScript
    , "Meetei_Mayek" ~> MeeteiMayekScript
    , "Mende_Kikakui" ~> MendeKikakuiScript
    , "Meroitic_Cursive" ~> MeroiticCursiveScript
    , "Meroitic_Hieroglyphs" ~> MeroiticHieroglyphsScript
    , "Miao" ~> MiaoScript
    , "Modi" ~> ModiScript
    , "Mongolian" ~> MongolianScript
    , "Mro" ~> MroScript
    , "Multani" ~> MultaniScript
    , "Myanmar" ~> MyanmarScript
    , "Nabataean" ~> NabataeanScript
    , "Nandinagari" ~> NandinagariScript
    , "Newa" ~> NewaScript
    , "New_Tai_Lue" ~> NewTaiLueScript
    , "Nko" ~> NkoScript
    , "Nushu" ~> NushuScript
    , "Nyiakeng_Puachue_Hmong" ~> NyiakengPuachueHmongScript
    , "Ogham" ~> OghamScript
    , "Ol_Chiki" ~> OlChikiScript
    , "Old_Hungarian" ~> OldHungarianScript
    , "Old_Italic" ~> OldItalicScript
    , "Old_North_Arabian" ~> OldNorthArabianScript
    , "Old_Permic" ~> OldPermicScript
    , "Old_Persian" ~> OldPersianScript
    , "Old_Sogdian" ~> OldSogdianScript
    , "Old_South_Arabian" ~> OldSouthArabianScript
    , "Old_Turkic" ~> OldTurkicScript
    , "Oriya" ~> OriyaScript
    , "Osage" ~> OsageScript
    , "Osmanya" ~> OsmanyaScript
    , "Pahawh_Hmong" ~> PahawhHmongScript
    , "Palmyrene" ~> PalmyreneScript
    , "Pau_Cin_Hau" ~> PauCinHauScript
    , "Phags_Pa" ~> PhagsPaScript
    , "Phoenician" ~> PhoenicianScript
    , "Psalter_Pahlavi" ~> PsalterPahlaviScript
    , "Rejang" ~> RejangScript
    , "Runic" ~> RunicScript
    , "Samaritan" ~> SamaritanScript
    , "Saurashtra" ~> SaurashtraScript
    , "Sharada" ~> SharadaScript
    , "Shavian" ~> ShavianScript
    , "Siddham" ~> SiddhamScript
    , "SignWriting" ~> SignWritingScript
    , "Sinhala" ~> SinhalaScript
    , "Sogdian" ~> SogdianScript
    , "Sora_Sompeng" ~> SoraSompengScript
    , "Soyombo" ~> SoyomboScript
    , "Sundanese" ~> SundaneseScript
    , "Syloti_Nagri" ~> SylotiNagriScript
    , "Syriac" ~> SyriacScript
    , "Tagalog" ~> TagalogScript
    , "Tagbanwa" ~> TagbanwaScript
    , "Tai_Le" ~> TaiLeScript
    , "Tai_Tham" ~> TaiThamScript
    , "Tai_Viet" ~> TaiVietScript
    , "Takri" ~> TakriScript
    , "Tamil" ~> TamilScript
    , "Tangut" ~> TangutScript
    , "Telugu" ~> TeluguScript
    , "Thaana" ~> ThaanaScript
    , "Thai" ~> ThaiScript
    , "Tibetan" ~> TibetanScript
    , "Tifinagh" ~> TifinaghScript
    , "Tirhuta" ~> TirhutaScript
    , "Ugaritic" ~> UgariticScript
    , "Unknown" ~> UnknownScript
    , "Vai" ~> VaiScript
    , "Wancho" ~> WanchoScript
    , "Warang_Citi" ~> WarangCitiScript
    , "Yi" ~> YiScript
    , "Zanabazar_Square" ~> ZanabazarSquareScript
    ]
  where
    (~>) = (,)
