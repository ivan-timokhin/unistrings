{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen
  ( Module(Module, moduleC, moduleHs)
  , generateEnum
  , EnumSpec(..)
  , generateIntegral
  , IntSpec(..)
  ) where

import Data.Bits (shiftL)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Vector as V
import Numeric (showHex)

import Gen.Type (IntegralType(itC, itHaskell))
import Trie (TrieDesc(Bottom, Layer))

data Module =
  Module
    { moduleC :: [ByteString]
    , moduleHs :: [ByteString]
    }
  deriving (Show)

data EnumSpec =
  EnumSpec
    { esCPrefix :: ByteString
    , esHsType :: ByteString
    , esHsTypeModule :: ByteString
    }

generateEnum ::
     Enum a
  => EnumSpec
  -> TrieDesc Identity IntegralType IntegralType a
  -> Module
generateEnum spec = generateIntG (enumSpec2IntGSpec spec)

enumSpec2IntGSpec :: Enum a => EnumSpec -> IntGSpec a
enumSpec2IntGSpec espec =
  IntGSpec
    { igsCPrefix = esCPrefix espec
    , igsHsType = esHsType espec
    , igsHsImports =
        [B.concat ["import ", esHsTypeModule espec, " (", esHsType espec, ")"]]
    , igsHsConvert = "toEnum . fromEnum $ "
    , igsConvert = toInteger . fromEnum
    }

data IntSpec =
  IntSpec
    { isCPrefix :: ByteString
    , isHsType :: ByteString
    }

generateIntegral ::
     Integral a
  => IntSpec
  -> TrieDesc Identity IntegralType IntegralType a
  -> Module
generateIntegral spec =
  generateIntG
    IntGSpec
      { igsCPrefix = isCPrefix spec
      , igsHsType = isHsType spec
      , igsHsImports = []
      , igsHsConvert = ""
      , igsConvert = toInteger
      }

data IntGSpec a =
  IntGSpec
    { igsCPrefix :: ByteString
    , igsHsType :: ByteString
    , igsHsImports :: [ByteString]
    , igsHsConvert :: ByteString
    , igsConvert :: a -> Integer
    }

generateIntG ::
     IntGSpec a -> TrieDesc Identity IntegralType IntegralType a -> Module
generateIntG = generateGeneric . intGSpec2Generic

intGSpec2Generic :: IntGSpec a -> GenericSpec IntegralType a
intGSpec2Generic ispec =
  GSpec
    { gsCPrefix = igsCPrefix ispec
    , gsCExtras = const []
    , gsHsType = igsHsType ispec
    , gsHsImports = igsHsImports ispec
    , gsHsFFI = []
    , gsHsConvert = \expr -> igsHsConvert ispec <> expr
    , gsHsIntegralType = id
    , gsConvert = igsConvert ispec
    }

data GenericSpec ann a =
  GSpec
    { gsCPrefix :: ByteString
    , gsCExtras :: ann -> [ByteString]
    , gsHsType :: ByteString
    , gsHsImports :: [ByteString]
    , gsHsFFI :: [ByteString]
    , gsHsConvert :: ByteString -> ByteString
    , gsHsIntegralType :: ann -> IntegralType
    , gsConvert :: a -> Integer
    }

generateGeneric ::
     GenericSpec ann a -> TrieDesc Identity IntegralType ann a -> Module
generateGeneric spec trie =
  Module
    { moduleC = generateGenericC spec trie
    , moduleHs = generateGenericHs spec trie
    }

generateGenericC ::
     forall a ann.
     GenericSpec ann a
  -> TrieDesc Identity IntegralType ann a
  -> [ByteString]
generateGenericC spec = (cHeader :) . go 0
  where
    go :: Foldable t => Int -> TrieDesc t IntegralType ann a -> [ByteString]
    go _ (Bottom ty xs) = generateBottom ty xs
    go lv (Layer ty _ layer rest) =
      renderCArray
        (itC ty)
        (gsCPrefix spec <> "_layer_" <> B.pack (show lv))
        (toList (Compose layer)) $
      go (lv + 1) rest
    generateBottom :: Foldable t => ann -> t (V.Vector a) -> [ByteString]
    generateBottom ann xs =
      renderCArray
        (itC (gsHsIntegralType spec ann))
        (gsCPrefix spec <> "_bottom")
        (map (gsConvert spec) $ toList (Compose xs)) $
      gsCExtras spec ann

generateGenericHs ::
     forall a bottomAnnotation.
     GenericSpec bottomAnnotation a
  -> TrieDesc Identity IntegralType bottomAnnotation a
  -> [ByteString]
generateGenericHs spec trie =
  header ++ [""] ++ foreignImports 0 trie ++ [""] ++ function
  where
    foreignImports ::
         Foldable t
      => Int
      -> TrieDesc t IntegralType bottomAnnotation a
      -> [ByteString]
    foreignImports _ (Bottom ty _) =
      renderHsCBinding
        (gsCPrefix spec <> "_bottom")
        "bottom"
        (itHaskell (gsHsIntegralType spec ty)) :
      gsHsFFI spec
    foreignImports lv (Layer ty _ _ rest) =
      renderHsCBinding
        (gsCPrefix spec <> "_layer_" <> B.pack (show lv))
        ("layer_" <> B.pack (show lv))
        (itHaskell ty) :
      foreignImports (lv + 1) rest
    header =
      "import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)" :
      "import Data.Bits ((.&.), shiftR, shiftL)" :
      "import Data.Int (Int8, Int16, Int32)" :
      "import Data.Word (Word8, Word16)" : gsHsImports spec
    function :: [ByteString]
    function = sig : "retrieve cp = val" : " where" : map ("  " <>) locals
      where
        sig = B.concat ["retrieve :: Int -> ", gsHsType spec]
        locals =
          case trie of
            Bottom _ (Identity _) ->
              [B.concat ["val = ", gsHsConvert spec "unsafeReadPtr bottom cp"]]
            Layer _ nbits _ rest ->
              B.concat
                [ "i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` "
                , B.pack $ show nbits
                ] :
              go 0 nbits rest
        go ::
             Int
          -> Int
          -> TrieDesc V.Vector IntegralType bottomAnnotation a
          -> [ByteString]
        go depth prevBits (Bottom _ _) =
          [ B.concat
              [ "val = "
              , gsHsConvert spec "unsafeReadPtr bottom $ i"
              , B.pack $ show depth
              , " `shiftL` "
              , B.pack $ show prevBits
              , " + cp .&. "
              , mask prevBits
              ]
          ]
        go depth prevBits (Layer _ bits _ rest) =
          B.concat
            [ "i"
            , B.pack $ show (depth + 1)
            , " = fromEnum $ unsafeReadPtr layer_"
            , B.pack $ show (depth + 1)
            , " $ i"
            , B.pack $ show depth
            , " `shiftL` "
            , B.pack $ show (prevBits - bits)
            , " + (cp `shiftR` "
            , B.pack $ show bits
            , ") .&. "
            , mask (prevBits - bits)
            ] :
          go (depth + 1) bits rest

renderCArray ::
     Show a => ByteString -> ByteString -> [a] -> [ByteString] -> [ByteString]
renderCArray ty name [] = (B.concat [ty, " const ", name, "[] = {};"] :)
renderCArray ty name [x] =
  (B.concat [ty, " const ", name, "[] = { ", B.pack (show x), " };"] :)
renderCArray ty name (x:xs) =
  (B.concat [ty, " const ", name, "[] = { ", B.pack (show x)] :) .
  (map (("  " <>) . foldMap (\x' -> ", " <> B.pack (show x'))) (chunksBy 10 xs) ++) .
  ("};" :)

renderHsCBinding :: ByteString -> ByteString -> ByteString -> ByteString
renderHsCBinding cname name hsType =
  B.concat ["foreign import ccall \"&", cname, "\" ", name, " :: Ptr ", hsType]

chunksBy :: Int -> [a] -> [[a]]
chunksBy _ [] = []
chunksBy n xs = chunk : chunksBy n rest
  where
    (chunk, rest) = splitAt n xs

cHeader :: ByteString
cHeader = "#include \"HsFFI.h\"\n"

mask :: Int -> ByteString
mask nbits = B.pack $ "0x" ++ showHex (((1 :: Int) `shiftL` nbits) - 1) ""
